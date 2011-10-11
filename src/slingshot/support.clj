(ns slingshot.support
  (:use [clojure.walk :only [prewalk-replace]])
  (:import slingshot.Stone))

;; try+ support

(defn throw-arg
  "Throws an IllegalArgumentException with a message. Args are like
  those of clojure.core/format."
  [fmt & args]
  (throw (IllegalArgumentException. (apply format fmt args))))

(defn clause-type
  "Returns a classifying value for any object in a try+ body:
  catch-clause, finally-clause, or other"
  [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn partition-body
  "Partitions a try+ body into exprs, catch-clauses, finally clauses,
  and a sentinel used for validating syntax"
  [body]
  (let [[e c f s] (partition-by clause-type body)
        [e c f s] (if (-> (first e) clause-type nil?) [e c f s] [nil e c f])
        [c f s] (if (-> (first c) clause-type (= 'catch)) [c f s] [nil c f])
        [f s] (if (-> (first f) clause-type (= 'finally)) [f s] [nil f])]
    [e c f s]))

(defn validate-try+-form
  "Throws if a partitioned try+ body is invalid"
  [exprs catch-clauses finally-clauses sentinel]
  (when (or sentinel (> (count finally-clauses) 1))

(defn validated-body-parts
  "Returns a validated set of try+ body parts"
  [body]
  (let [[e c f s] (partition-body body)]
    (validate-try+-form e c f s)
    [e c f]))

(defn resolved
  "For symbols, returns the resolved value or throws if not resolvable"
  [x]
  (when (symbol? x)
    (or (resolve x)
        (throw-arg "Unable to resolve symbol: %s in this context" x))))

(defn ns-qualify
  "Returns a fully qualified symbol with the same name as the
  argument, and a namespace part that refers to the current
  namespace."
  [sym]
  (-> *ns* ns-name name (symbol (name sym))))

(defn catch->cond
  "Converts a try+ catch-clause into a test/expr pair for cond"
  [[_ selector binding-form & exprs]]
  [(cond (class? (resolved selector))
         `(instance? ~selector (:object ~'&throw-context))
         (vector? selector)
         (let [[key val] selector]
           `(= (get (:object ~'&throw-context) ~key) ~val))
         (seq? selector)
         (prewalk-replace {(ns-qualify '%) '(:object &throw-context)} selector)
         :else
         `(~selector (:object ~'&throw-context)))
   `(let [~binding-form (:object ~'&throw-context)]
      ~@exprs)])

(defn throwable->context
  "Returns the context map associated with a Throwable t. If t or any
  throwable in its cause chain is a Stone, returns its context, else
  returns a new context with t as the thrown object."
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

  Normal processing by catch clauses can be preempted by adding
  special keys to the metadata on the returned context map:

  If the metadata contains the key:
    - :catch-hook-return, try+ will return the corresponding value;
    - :catch-hook-throw, try+ will throw+ the corresponding value.

  Defaults to identity."}
  *catch-hook* identity)

(defn try-compatible-catch
  "Transforms a seq of try+ catch-clauses into a single try-compatible
  catch. throw-fn must name a function that can accept zero or one
  arguments. It will be called at runtime with one argument if the
  *catch-hook* requests :catch-hook-throw or with zero arguments if
  none of the catch claues matches."
  [catch-clauses throw-fn]
  ;; the code below uses only one local to minimize clutter in the
  ;; &env captured by throw+ forms within catch clauses (see the
  ;; special handling of &throw-context in throw+)
  `(catch Throwable ~'&throw-context
     (let [~'&throw-context (-> ~'&throw-context throwable->context
                                *catch-hook*)]
       (cond
        (contains? (meta ~'&throw-context) :catch-hook-return)
        (:catch-hook-return (meta ~'&throw-context))
        (contains? (meta ~'&throw-context) :catch-hook-throw)
        (~throw-fn (:catch-hook-throw (meta ~'&throw-context)))
        ~@(mapcat catch->cond catch-clauses)
        :else
        (~throw-fn)))))

(defn transform-catch-clauses
  [catch-clauses throw-fn]
  (when catch-clauses
    [(try-compatible-catch catch-clauses throw-fn)]))

;; throw+ support

(defn make-stack-trace
  "Returns the current stack trace beginning at the caller's frame"
  []
  (let [trace (.getStackTrace (Thread/currentThread))]
    (java.util.Arrays/copyOfRange trace 2 (alength trace))))

(defn make-throwable
  "Returns a throwable Stone that wraps the given a message, cause,
  stack-trace, and context"
  [message cause stack-trace context]
  (Stone. message cause stack-trace context))

(defn context-message
  "Returns the default message string for a throw context"
  [{:keys [message object]}]
  (str message ": " (pr-str object)))

(defn context->throwable
  "If object in context is a Throwable, returns it, else wraps it and
   returns the wrapper."
  [{:keys [object cause stack-trace] :as context}]
  (if (instance? Throwable object)
    object
    (make-throwable (context-message context) cause stack-trace context)))

(defn default-throw-hook
  "Default implementation of *throw-hook*. If object in context is a
  Throwable, throws it, else wraps it and throws the wrapper."
  [context]
  (throw (context->throwable context)))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. Defaults to
  default-throw-hook."}
  *throw-hook* default-throw-hook)

(defn throw-context
  "Throws a context. Allows overrides of *throw-hook* to intervene."
  [context]
  (*throw-hook* context))
