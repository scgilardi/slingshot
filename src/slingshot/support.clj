(ns slingshot.support
  (:use [clojure.walk :only [prewalk-replace]])
  (:import slingshot.Stone))

(defn throw-arg
  "Throws an IllegalArgumentException with a message specified by a
  format string and args for clojure.core/format"
  [fmt & args]
  (throw (IllegalArgumentException. (apply format fmt args))))

;; try+ support

(defn try-item-type
  "Returns a classifying keyword for an item in a try+ body: :expression,
  :catch-clause, or :finally-clause"
  [item]
  ({'catch :catch-clause 'finally :finally-clause}
   (and (seq? item) (first item))
   :expression))

(defn match-or-defer
  "Takes a seq of seqs of try items and a try item type. If the first
  item in the first seq has that item type, returns the seq, else
  returns the seq prepended with nil"
  [s type]
  (if (-> s ffirst try-item-type (= type))
    s
    (cons nil s)))

(defn parse-try
  "Returns a vector of seqs containing the expressions, catch clauses,
  and finally clauses in a try+ body, or throws if the body's structure
  is invalid"
  [body]
  (let [groups (partition-by try-item-type body)
        [e & groups] (match-or-defer groups :expression)
        [c & groups] (match-or-defer groups :catch-clause)
        [f & groups] (match-or-defer groups :finally-clause)]
    (if (and (nil? groups) (<= (count f) 1))
      [e c f]
      (throw-arg "try+ form must match: %s"
                 "(try+ expression* catch-clause* finally-clause?)"))))

;; catch support

(defn selector-type
  "Returns a classifying keyword for a selector: :class-name, :key-value,
  :form, or :predicate"
  [selector]
  (cond
   (and (symbol? selector) (class? (resolve selector))) :class-name
   (vector? selector) :key-value
   (seq? selector) :form
   :else :predicate))

(defn parse-key-value
  "Returns a pair: the key and value for a key-value selector, or throws if
  the selector's structure is invald"
  [[key val :as selector]]
  (if (= (count selector) 2)
    [key val]
    (throw-arg "key-value selector: %s does not match: [key val]"
               (pr-str selector))))

(defn cond-test
  "Returns the test part of a cond test/expression pair given a selector"
  [selector]
  (prewalk-replace
   {'% '(:object &throw-context)}
   (case (selector-type selector)
     :class-name `(instance? ~selector ~'%)
     :key-value (let [[key val] (parse-key-value selector)]
                  `(= (get ~'% ~key) ~val))
     :predicate `(~selector ~'%)
     :form selector)))

(defn cond-expression
  "Returns the expression part of a cond test/expression pair given a
  binding form and seq of expressions"
  [binding-form expressions]
  `(let [~binding-form (:object ~'&throw-context)]
     ~@expressions))

(defn cond-test-expression
  "Converts a try+ catch-clause into a test/expression pair for cond"
  [[_ selector binding-form & expressions]]
  [(cond-test selector) (cond-expression binding-form expressions)])

(defn ->context
  "Returns a context given a Throwable t. If t or any Throwable in its
  cause chain is a Stone, returns the Stone's context with t assoc'd
  as the value for :wrapper, else returns a new context with t as the
  thrown object."
  [throwable]
  (-> (loop [cause throwable]
        (cond (instance? Stone cause)
              (assoc (.getContext cause) :wrapper throwable)
              (.getCause cause)
              (recur (.getCause cause))
              :else
              {:object throwable
               :message (.getMessage throwable)
               :cause (.getCause throwable)
               :stack-trace (.getStackTrace throwable)}))
      (with-meta {:throwable throwable})))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of catch. Must be
  bound to a function of one argument, a context map with metadata.
  Returns a (possibly modified) context map to be considered by catch
  clauses. Existing metadata on the context map must be preserved (or
  intentionally modified) in the returned context map.

  Normal processing by catch clauses can be skipped by adding special
  keys to the metadata on the returned context map:

  If the metadata contains the key:
    - :catch-hook-return, try+ will return the corresponding value;
    - :catch-hook-throw, try+ will throw+ the corresponding value;
    - :catch-hook-rethrow, try+ will rethrow the caught object's
      outermost wrapper.

  Defaults to identity."}
  *catch-hook* identity)

(defn transform
  "Transforms a seq of catch clauses for try+ into a seq of catch
  clauses for try that implements the specified behavior. throw-sym
  names a macro or function (usually throw+) that can accept zero or
  one arguments. It is called with one argument for :catch-hook-throw
  requests, or zero arguments for :catch-hook-rethrow requests or when
  no try+ catch clause matches."
  [catch-clauses throw-sym]
  ;; the code below uses only one local to minimize clutter in the
  ;; &env captured by throw+ forms within catch clauses (see the
  ;; special handling of &throw-context in make-context)
  (when catch-clauses
    (list
     `(catch Throwable ~'&throw-context
        (let [~'&throw-context (-> ~'&throw-context ->context *catch-hook*)]
          (cond
           (contains? (meta ~'&throw-context) :catch-hook-return)
           (:catch-hook-return (meta ~'&throw-context))
           (contains? (meta ~'&throw-context) :catch-hook-throw)
           (~throw-sym (:catch-hook-throw (meta ~'&throw-context)))
           (contains? (meta ~'&throw-context) :catch-hook-rethrow)
           (~throw-sym)
           ~@(mapcat cond-test-expression catch-clauses)
           :else
           (~throw-sym)))))))

;; throw+ support

(defn stack-trace
  "Returns the current stack trace beginning at the caller's frame"
  []
  (let [trace (.getStackTrace (Thread/currentThread))]
    (java.util.Arrays/copyOfRange trace 2 (alength trace))))

(defmacro environment
  "Expands to code that generates a map of locals: names to values"
  []
  `(zipmap '~(keys &env) [~@(keys &env)]))

(defn throwable-message
  "Returns the context's message augmented with a printed
  representation of the thrown object"
  [{:keys [message object]}]
  (str message ": " (pr-str object)))

(defn make-throwable
  "Returns a throwable Stone that wraps context"
  [{:keys [message cause stack-trace] :as context}]
  (Stone. (throwable-message context) cause stack-trace context))

(defn ->throwable
  "Returns a throwable given a context: the object in context if it's
  a Throwable, else a throwable Stone that wraps context"
  [{object :object :as context}]
  (if (instance? Throwable object)
    object
    (make-throwable context)))

(defn default-throw-hook
  "Default implementation of *throw-hook*"
  [context]
  (throw (->throwable context)))

(defn rethrow
  "Rethrows the Throwable that try caught"
  [context]
  (throw (-> context meta :throwable)))

(defn make-context
  "Makes a throw context from arguments. Captures the cause if called
  within a catch clause."
  [object message stack-trace environment]
  {:object object
   :message message
   :cause (-> (environment '&throw-context) meta :throwable)
   :stack-trace stack-trace
   :environment (dissoc environment '&throw-context)})

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. Defaults to
  default-throw-hook."}
  *throw-hook* default-throw-hook)

(defn throw-context
  "Throws a context. Allows overrides of *throw-hook* to intervene."
  [object message stack-trace environment]
  (*throw-hook* (make-context object message stack-trace environment)))
