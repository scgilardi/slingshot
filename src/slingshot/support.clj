(ns slingshot.support
  (:require [clojure.walk]))

(defn replace-all
  "Returns a deep copy of coll with all instances of the keys in smap
  replaced by their values"
  [smap coll]
  (clojure.walk/postwalk-replace smap coll))

(defn throw-arg
  "Throws an IllegalArgumentException with a message specified by a
  arguments for clojure.core/format"
  [fmt & args]
  (throw (IllegalArgumentException. (apply format fmt args))))

;; try+ support

(defn parse-try+
  "Returns a vector of seqs containing the expressions, catch clauses,
  and finally clauses in a try+ body, or throws if the body's structure
  is invalid"
  [body]
  (letfn
      [(item-type [item]
         ({'catch :catch-clause 'finally :finally-clause}
          (and (seq? item) (first item))
          :expression))
       (match-or-defer [s type]
         (if (-> s ffirst item-type (= type)) s (cons nil s)))]
    (let [groups (partition-by item-type body)
          [e & groups] (match-or-defer groups :expression)
          [c & groups] (match-or-defer groups :catch-clause)
          [f & groups] (match-or-defer groups :finally-clause)]
      (if (and (nil? groups) (<= (count f) 1))
        [e c f]
        (throw-arg "try+ form must match: %s"
                   "(try+ expression* catch-clause* finally-clause?)")))))

(defn find-context
  "Searches throwable and its cause chain for a Stone. If one is
  found, returns the context it wraps, else returns nil."
  [throwable]
  (if (instance? slingshot.Stone throwable)
    (.getContext throwable)
    (when-let [cause (.getCause throwable)]
      (recur cause))))

(defn ->context
  "Returns a context given a Throwable t. If t or any Throwable in its
  cause chain is a Stone, returns the Stone's context with t assoc'd
  as the value for :throwable, else returns a new context based on t."
  [throwable]
  (-> (or (find-context throwable)
          {:object throwable
           :message (.getMessage throwable)
           :cause (.getCause throwable)
           :stack-trace (.getStackTrace throwable)})
      (assoc :throwable throwable)))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of catch. Must be
  bound to a function of one argument, a context map. Returns
  a (possibly modified) context map to be considered by catch clauses.

  Normal processing by catch clauses can be skipped by adding special
  keys to the context map:

  If the context contains the key:
    - :catch-hook-return, try+ will return the corresponding value;
    - :catch-hook-throw, try+ will throw+ the corresponding value;
    - :catch-hook-rethrow, try+ will rethrow the caught object's
      outermost wrapper.

  Defaults to identity."}
  *catch-hook* identity)

(defn transform-catch
  "Transforms a seq of catch clauses for try+ into a seq of one catch
  clause for try that implements the specified behavior. throw-sym
  names a macro or function (usually throw+) that can accept zero or
  one arguments. It is called with one argument for :catch-hook-throw
  requests, or zero arguments for :catch-hook-rethrow requests or when
  no try+ catch clause matches."
  [catch-clauses throw-sym]
  (letfn
      [(cond-test [selector]
         (letfn
             [(class-name []
                (and (symbol? selector) (class? (resolve selector))
                     `(instance? ~selector ~'%)))
              (key-value []
                (and (vector? selector)
                     (if (= (count selector) 2)
                       (let [[key val] selector]
                         `(= (get ~'% ~key) ~val))
                       (throw-arg "key-value selector: %s does not match: %s"
                                  (pr-str selector) "[key val]"))))
              (selector-form []
                (and (seq? selector) selector))
              (predicate []
                `(~selector ~'%))]
           (->> (or (class-name) (key-value) (selector-form) (predicate))
                (replace-all {'% '(:object &throw-context)}))))
       (cond-expression [binding-form expressions]
         `(let [~binding-form (:object ~'&throw-context)]
            ~@expressions))
       (transform [[_ selector binding-form & expressions]]
         [(cond-test selector) (cond-expression binding-form expressions)])]
    (when catch-clauses
      (list
       ;; the code below uses only one local name to minimize clutter
       ;; in the &env captured by throw+ forms within catch clauses
       ;; (see the special handling of &throw-context in make-context)
       `(catch Throwable ~'&throw-context
          (let [~'&throw-context (-> ~'&throw-context ->context *catch-hook*)]
            (cond
             (contains? ~'&throw-context :catch-hook-return)
             (:catch-hook-return ~'&throw-context)
             (contains? ~'&throw-context :catch-hook-throw)
             (~throw-sym (:catch-hook-throw ~'&throw-context))
             (contains? ~'&throw-context :catch-hook-rethrow)
             (~throw-sym)
             ~@(mapcat transform catch-clauses)
             :else
             (~throw-sym))))))))

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

(defn wrap
  "Returns a throwable Stone that wraps context"
  [{:keys [message cause stack-trace] :as context}]
  (slingshot.Stone. message cause stack-trace context))

(defn ->throwable
  "Returns a throwable given a context: the object in context if it's
  a Throwable, else a throwable Stone that wraps context"
  [{object :object :as context}]
  (if (instance? Throwable object)
    object
    (wrap context)))

(defn default-throw-hook
  "Default implementation of *throw-hook*"
  [context]
  (throw (->throwable context)))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. Defaults to
  default-throw-hook."}
  *throw-hook* default-throw-hook)

(defn make-context
  "Makes a throw context from arguments. Captures the cause if called
  within a try+ catch clause."
  [object message stack-trace environment]
  {:object object
   :message message
   :cause (:throwable (environment '&throw-context))
   :stack-trace stack-trace
   :environment (dissoc environment '&throw-context)})

(defn throw-context
  "Throws a context. Allows overrides of *throw-hook* to intervene."
  [object message stack-trace environment]
  (*throw-hook* (make-context object message stack-trace environment)))

(defmacro rethrow
  "Within a try+ catch clause, throws the outermost wrapper of the
  caught object"
  []
  `(throw (:throwable ~'&throw-context)))
