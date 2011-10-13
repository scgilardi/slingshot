(ns slingshot.support
  (:use [clojure.walk :only [prewalk-replace]])
  (:import slingshot.Stone))

;; try+ support

(defn throw-arg
  "Throws an IllegalArgumentException with a message specified by args like
  those of clojure.core/format."
  [fmt & args]
  (throw (IllegalArgumentException. (apply format fmt args))))

(defn part-type
  "Returns a classifying symbol for an item in a try+ body: 'expr,
  'catch, or 'finally"
  [x]
  (or (and (seq? x) (-> x first #{'catch 'finally})) 'expr))

(defn parse
  "Returns a vector of seqs containing the exprs, catch clauses, and
  finally clauses in a try+ body, or throws if the body's structure is
  invalid"
  [body]
  (let [[p q r s] (partition-by part-type body)
        [e q r s] (if (-> p first part-type (= 'expr)) [p q r s] [nil p q r])
        [c r s] (if (-> q first part-type (= 'catch)) [q r s] [nil q r])
        [f s] (if (-> r first part-type (= 'finally)) [r s] [nil r])]
    (if (and (nil? s) (<= (count f) 1))
      [e c f]
      (throw-arg "try+ form must match: %s"
                 "(try+ expr* catch-clause* finally-clause?)"))))

(defn class-name?
  "Returns true if the argument is a symbol that resolves to a Class
  in the current namespace"
  [x]
  (and (symbol? x) (class? (resolve x))))

(defn ns-qualify
  "Returns a fully qualified symbol with the same name as the
  argument, but \"in\" the current namespace"
  [sym]
  (-> *ns* ns-name name (symbol (name sym))))

(defn catch->cond
  "Converts a try+ catch-clause into a test/expr pair for cond"
  [[_ selector binding-form & exprs]]
  [(cond (class-name? selector)
         `(instance? ~selector (:object ~'&throw-context))
         (vector? selector)
         (let [[key val & sentinel] selector]
           (if sentinel
             (throw-arg "key-value selector: %s does not match: [key val]"
                        (pr-str selector))
             `(= (get (:object ~'&throw-context) ~key) ~val)))
         (seq? selector)
         (prewalk-replace {(ns-qualify '%) '(:object &throw-context)} selector)
         :else ;; predicate
         `(~selector (:object ~'&throw-context)))
   `(let [~binding-form (:object ~'&throw-context)]
      ~@exprs)])

(defn throwable->context
  "Returns a context map based on Throwable t. If t or any Throwable
  in its cause chain is a Stone, returns its context with t assoc'd as
  the value for :wrapper, else returns a new context with t as the
  thrown object."
  [t]
  (-> (loop [c t]
        (cond (instance? Stone c)
              (assoc (.getContext c) :wrapper t)
              (.getCause c)
              (recur (.getCause c))
              :else
              {:object t
               :message (.getMessage t)
               :cause (.getCause t)
               :stack-trace (.getStackTrace t)}))
      (with-meta {:throwable t})))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of catch. Must be
  bound to a function of one argument, a context map with
  metadata. Returns a (possibly modified) context map to be considered
  by catch clauses. Existing metadata on the context map must be
  preserved (or intentionally modified) in the returned context map.

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
        (let [~'&throw-context (-> ~'&throw-context throwable->context
                                   *catch-hook*)]
          (cond
           (contains? (meta ~'&throw-context) :catch-hook-return)
           (:catch-hook-return (meta ~'&throw-context))
           (contains? (meta ~'&throw-context) :catch-hook-throw)
           (~throw-sym (:catch-hook-throw (meta ~'&throw-context)))
           (contains? (meta ~'&throw-context) :catch-hook-rethrow)
           (~throw-sym)
           ~@(mapcat catch->cond catch-clauses)
           :else
           (~throw-sym)))))))

;; throw+ support

(defn stack-trace
  "Returns the current stack trace beginning at the caller's frame"
  []
  (let [trace (.getStackTrace (Thread/currentThread))]
    (java.util.Arrays/copyOfRange trace 2 (alength trace))))

(defmacro env-map
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

(defn context->throwable
  "If object in context is a Throwable, returns it, else wraps it and
  returns the wrapper."
  [{object :object :as context}]
  (if (instance? Throwable object)
    object
    (make-throwable context)))

(defn default-throw-hook
  "Default implementation of *throw-hook*"
  [context]
  (throw (context->throwable context)))

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
