(ns slingshot.support
           (:refer-clojure :exclude [format])
           (:require
             [cljs.analyzer]
             [clojure.string :as str]
             [slingshot.util
               :refer [appears-within?
                       #?@(:clj [if-cljs when-cljs])]])
  #?(:cljs (:require-macros
             [slingshot.util :refer [if-cljs when-cljs]]))
  #?(:cljs (:import goog.string goog.string.format)))

(def format #?(:clj  clojure.core/format
               :cljs goog.string/format))

(defn throw-arg
  "Throws an IllegalArgumentException with a message given arguments
  for clojure.core/format"
  [fmt & args]
  (throw (#?(:clj  IllegalArgumentException.
             :cljs js/Error.) ^String (apply format fmt args))))

;; context support

(defn make-context
  "Makes a throw context from a Throwable or explicit arguments"
  ([t]
    (make-context t
      #?(:clj (.getMessage    ^Throwable t) :cljs (.-message t))
      #?(:clj (.getCause      ^Throwable t) :cljs (.-cause   t))
      #?(:clj (.getStackTrace ^Throwable t) :cljs (.-stack   t))))
  ([object message cause stack-trace]
   {:object      object
    :message     message
    :cause       cause
    :stack-trace stack-trace}))

(defn wrap
  "Returns a context wrapper given a context"
  [{:keys [object message cause stack-trace]}]
  (let [data (if (map? object) object ^::wrapper? {:object object})
        ex   (ex-info message data cause)]
    #?(:clj  (doto ^Throwable ex (.setStackTrace stack-trace))
       :cljs ex)))

(defn unwrap
  "If t is a context wrapper or other IExceptionInfo, returns the
  corresponding context with t assoc'd as the value for :wrapper, else
  returns nil"
  [t]
  (if-let [data (ex-data t)]
    (assoc (make-context t)
      :object (if (::wrapper? (meta data)) (:object data) data)
      :wrapper t)))

(defn unwrap-all
  "Searches Throwable t and its cause chain for a context wrapper or
  other IExceptionInfo. If one is found, returns the corresponding
  context with the wrapper assoc'd as the value for :wrapper, else
  returns nil."
  [t]
  (or (unwrap t)
      (when-let [cause (identity #?(:clj  (.getCause ^Throwable t)
                                    :cljs (.-cause t)))]
        (recur cause))))

(defn get-throwable
  "Returns a Throwable given a context: the object in context if it's
  a Throwable, else a Throwable context wrapper"
  [{object :object :as context}]
  (if (instance? #?(:clj Throwable :cljs js/Error) object)
    object
    (wrap context)))

(defn get-context
  "Returns a context given a Throwable t. If t or any Throwable in its
  cause chain is a context wrapper or other IExceptionInfo, returns
  the corresponding context with the wrapper assoc'd as the value
  for :wrapper and t assoc'd as the value for :throwable. Otherwise
  creates a new context based on t with t assoc'd as the value
  for :throwable."
  [t]
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

(defn js-sym->sym
  "Demunges a JS variable name into a Clojure symbol.

   Example: my_ns.sub_ns.myvar -> my-ns.sub-ns/myvar"
  [x]
  (let [x-str (str x)
        dot-i (.lastIndexOf ^String x-str ".")
        [ns* name*]
          (->> [(->> x-str (take dot-i      ))
                (->> x-str (drop (inc dot-i)))]
               (map (partial apply str))
               (map #?(:clj clojure.main/demunge :cljs cljs.core/demunge)))]
    (symbol ns* name*)))

(defn gen-catch
  "Transforms a seq of catch clauses for try+ into a list containing
  one catch clause for try that implements the specified behavior.
  throw-sym names a macro or function (usually throw+) that can accept
  zero or one arguments. It is called with one argument
  for :catch-hook-throw requests, or zero arguments
  for :catch-hook-rethrow requests or when no try+ catch clause
  matches."
  [env catch-clauses throw-sym threw?-sym]
  (letfn
      [(resolve* [sym]
         (if-cljs env
            (let [found (cljs.analyzer/resolve-existing-var env sym)
                  found' (delay (cljs.analyzer/resolve-var env (js-sym->sym sym)))]
              (cond (or (:type found) (:meta found))
                    found
                    (or (:type @found') (:meta @found'))
                    @found'
                    :else found))
            (#?(:clj resolve) sym)))
       (class?* [x]
         (if-cljs env
           (or (:type x) (:record x)
               (and (-> x :ns (= 'js)) ; could be a JS class but can't determine at compile-time
                    (-> x :meta not)))
           (#?(:clj class?) x)))
       (class-selector? [selector]
         (when (symbol? selector)
           (let [resolved (resolve* selector)]
             (when (class?* resolved)
               (if-cljs env selector resolved)))))
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
     `(catch ~(if-cljs env :default `Throwable) ~'&throw-context
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
              ~(if finally-clause
                 finally-clause))))
        finally-clause
        (list finally-clause)))

;; throw+ support

#?(:clj
(defmacro resolve-local
  "Expands to sym if it names a local in the current environment or
  nil otherwise"
  [sym]
  (when (contains? (if-cljs &env (:locals &env) &env) sym)
    sym)))

(defn stack-trace
  "Returns the current stack trace beginning at the caller's frame"
  []
  #?(:clj  (let [trace (.getStackTrace (Thread/currentThread))]
             (java.util.Arrays/copyOfRange trace 1 (alength trace)))
     :cljs (-> (js/Error.) .-stack)))

(defn parse-throw+
  "Returns a vector containing the message and cause that result from
  processing the arguments to throw+"
  [object cause & args]
  (let [[cause & args] (if (or (empty? args) (string? (first args)))
                         (cons cause args)
                         args)
        [fmt & args] (cond (next args)
                           args
                           (seq args)
                           ["%s" (first args)]
                           :else
                           ["throw+: %s" (pr-str object)])
        message (apply format fmt args)]
    [message cause]))

(defn default-throw-hook
  "Default implementation of *throw-hook*"
  [context]
  (throw (get-throwable context)))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. Defaults to
  default-throw-hook."}
  *throw-hook* default-throw-hook)

(defn throw-fn
  "Helper to throw a context based on arguments and &env from throw+"
  [object {cause :throwable} stack-trace & args]
  (let [[message cause] (apply parse-throw+ object cause args)
        context (make-context object message cause stack-trace)]
    (*throw-hook* context)))

(defmacro rethrow
  "Within a try+ catch clause, throws the outermost wrapper of the
  caught object"
  []
  `(throw (:throwable ~'&throw-context)))
