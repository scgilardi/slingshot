(ns slingshot.support
  (:require [clojure.walk]))

(defn appears-within?
  "Returns true if x appears within coll at any nesting depth"
  [x coll]
  (let [result (atom false)]
    (clojure.walk/postwalk
     (fn [t]
       (when (= x t)
         (reset! result true)))
     coll)
    @result))

(defn throw-arg
  "Throws an IllegalArgumentException with a message given arguments
  for clojure.core/format"
  [fmt & args]
  (throw (IllegalArgumentException. ^String (apply format fmt args))))

;; context support

(defn make-context
  "Makes a throw context from arguments. Captures the cause from the
  prev-context argument if present."
  ([^Throwable t]
     {:object t
      :message (.getMessage t)
      :cause (.getCause t)
      :stack-trace (.getStackTrace t)})
  ([object message stack-trace prev-context]
     {:object object
      :message message
      :cause (:throwable prev-context)
      :stack-trace stack-trace}))

(defn wrap
  "Returns a context wrapper given a context"
  [context]
  (let [{:keys [message cause stack-trace]} context
        data (-> (dissoc context :message :cause :stack-trace)
                 (vary-meta assoc ::wrapper? true))
        ^Throwable wrapper (ex-info message data cause)]
    (doto wrapper
      (.setStackTrace stack-trace))))

(defn unwrap
  "If t is a context wrapper, returns the context with t assoc'd as
  the value for :wrapper, else returns nil"
  [^Throwable t]
  (when-let [data (ex-data t)]
    (when (::wrapper? (meta data))
      (-> (assoc data
            :message (.getMessage t)
            :cause (.getCause t)
            :stack-trace (.getStackTrace t)
            :wrapper t)
          (vary-meta dissoc ::wrapper?)))))

(defn unwrap-all
  "Searches Throwable t and its cause chain for a context wrapper. If
  one is found, returns the context with the wrapper assoc'd as the
  value for :wrapper, else returns nil."
  [^Throwable t]
  (or (unwrap t)
      (when-let [cause (.getCause t)]
        (recur cause))))

(defn get-throwable
  "Returns a Throwable given a context: the object in context if it's
  a Throwable, else a Throwable context wrapper"
  [{object :object :as context}]
  (if (instance? Throwable object)
    object
    (wrap context)))

(defn get-context
  "Returns a context given a Throwable t. If t or any Throwable in its
  cause chain is a context wrapper, returns the context with the
  wrapper assoc'd as the value for :wrapper and t assoc'd as the value
  for :throwable. Otherwise creates a new context based on t with t
  assoc'd as the value for :throwable."
  [^Throwable t]
  (-> (or (unwrap-all t)
          (make-context t))
      (assoc :throwable t)))

;; try+ support

(defn parse-try+
  "Returns a vector of seqs containing the expressions, catch clauses,
  and finally clauses in a try+ body, or throws if the body's structure
  is invalid"
  [body]
  (letfn
      [(item-type [item]
         ({'catch :catch-clause 'else :else-clause 'finally :finally-clause}
          (and (seq? item) (first item))
          :expression))
       (match-or-defer [s type]
         (if (-> s ffirst item-type (= type)) s (cons nil s)))]
    (let [groups (partition-by item-type body)
          [e & groups] (match-or-defer groups :expression)
          [c & groups] (match-or-defer groups :catch-clause)
          [l & groups] (match-or-defer groups :else-clause)
          [f & groups] (match-or-defer groups :finally-clause)]
      (if (every? nil? [groups (next l) (next f)])
        [e c (first l) (first f)]
        (throw-arg "try+ form must match: (%s %s)"
                   "try+ expression* catch-clause*"
                   "else-clause? finally-clause?")))))

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

(defn gen-catch
  "Transforms a seq of catch clauses for try+ into a list containing
  one catch clause for try that implements the specified behavior.
  throw-sym names a macro or function (usually throw+) that can accept
  zero or one arguments. It is called with one argument
  for :catch-hook-throw requests, or zero arguments
  for :catch-hook-rethrow requests or when no try+ catch clause
  matches."
  [catch-clauses throw-sym threw?-sym]
  (letfn
      [(class-selector? [selector]
         (if (symbol? selector)
           (let [resolved (resolve selector)]
             (if (class? resolved)
               resolved))))
       (cond-test [selector]
         (letfn
             [(key-values []
                (and (vector? selector)
                     (if (even? (count selector))
                       `(and ~@(for [[key val] (partition 2 selector)]
                                 `(= (get ~'% ~key) ~val)))
                       (throw-arg "key-values selector: %s does not match: %s"
                                  (pr-str selector) "[key val & kvs]"))))
              (selector-form []
                (and (seq? selector) (appears-within? '% selector)
                     selector))
              (predicate []
                `(~selector ~'%))]
           `(let [~'% (:object ~'&throw-context)]
              ~(or (key-values) (selector-form) (predicate)))))
       (cond-expression [binding-form expressions]
         `(let [~binding-form (:object ~'&throw-context)]
            ~@expressions))
       (transform [[_ selector binding-form & expressions]]
         (if-let [class-selector (class-selector? selector)]
           [`(instance? ~class-selector (:object ~'&throw-context))
            (cond-expression (with-meta binding-form {:tag selector}) expressions)]
           [(cond-test selector) (cond-expression binding-form expressions)]))]
    (list
     ;; the code below uses only one local name to minimize clutter
     ;; in the &env captured by throw+ forms within catch clauses
     ;; (see the special handling of &throw-context in make-context)
     `(catch Throwable ~'&throw-context
        (reset! ~threw?-sym true)
        (let [~'&throw-context (-> ~'&throw-context get-context *catch-hook*)]
          (cond
           (contains? ~'&throw-context :catch-hook-return)
           (:catch-hook-return ~'&throw-context)
           (contains? ~'&throw-context :catch-hook-throw)
           (~throw-sym (:catch-hook-throw ~'&throw-context))
           (contains? ~'&throw-context :catch-hook-rethrow)
           (~throw-sym)
           ~@(mapcat transform catch-clauses)
           :else
           (~throw-sym)))))))

(defn gen-finally
  "Returns either nil or a list containing a finally clause for a try
  form based on the parsed else and/or finally clause from a try+
  form"
  [else-clause finally-clause threw?-sym]
  (cond else-clause
        (list
         `(finally
            (try
              (when-not @~threw?-sym
                ~@(rest else-clause))
              ~(when finally-clause
                 finally-clause))))
        finally-clause
        (list finally-clause)))

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

(defn default-throw-hook
  "Default implementation of *throw-hook*"
  [context]
  (throw (get-throwable context)))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. Defaults to
  default-throw-hook."}
  *throw-hook* default-throw-hook)

(defn throw-context
  "Throws a context. Allows overrides of *throw-hook* to intervene."
  [object message stack-trace prev-context]
  (*throw-hook* (make-context object message stack-trace prev-context)))

(defmacro rethrow
  "Within a try+ catch clause, throws the outermost wrapper of the
  caught object"
  []
  `(throw (:throwable ~'&throw-context)))
