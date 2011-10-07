(ns slingshot.support
  (:use [clojure.walk :only [prewalk-replace]])
  (:import slingshot.Stone))

(defn clause-type
  "Returns a classifying value for any object in a try+ body:
  catch-clause, finally-clause, or other"
  [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn partition-body
  "Partitions a try+ body into exprs, catch-clauses, finally
  clauses, and a sentinel used for validating syntax"
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
    (throw (IllegalArgumentException.
            (format "try+ form must match: (try+ %s)"
                    "expr* catch-clause* finally-clause?")))))

(defn throw-context
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

(defn resolved
  "For symbols, returns the resolved value or throws if not resolvable"
  [x]
  (when (symbol? x)
    (or (resolve x)
        (throw (IllegalArgumentException.
                (str "Unable to resolve symbol: " x " in this context"))))))

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
         (seq? selector)
         (prewalk-replace {(ns-qualify '%) '(:object &throw-context)} selector)
         :else
         `(~selector (:object ~'&throw-context)))
   `(let [~binding-form (:object ~'&throw-context)]
      ~@exprs)])

(defn transform-catch
  "Transforms a seq of try+ catch-clauses and a default into a single
  try-compatible catch"
  [catch-clauses default]
  ;; the code below uses only one local to minimize clutter in the
  ;; &env captured by throw+ forms within catch clauses (see the
  ;; special handling of &throw-context in throw+)
  `(catch Throwable ~'&throw-context
     (let [~'&throw-context (-> ~'&throw-context throw-context
                                slingshot.hooks/*catch-hook*)]
       (cond
        (contains? (meta ~'&throw-context) :catch-hook-return)
        (:catch-hook-return (meta ~'&throw-context))
        (contains? (meta ~'&throw-context) :catch-hook-throw)
        (slingshot.core/throw+ (:catch-hook-throw (meta ~'&throw-context)))
        ~@(mapcat catch->cond catch-clauses)
        :else ~default))))

(defn make-throwable
  "Returns a throwable Stone that wraps the given a message, cause,
  stack-trace, and context"
  [message cause stack-trace context]
  (Stone. message cause stack-trace context))

(defn context-message
  "Returns the default message string for a throw context"
  [{:keys [message object]}]
  (str message ": " (pr-str object)))

(defn default-throw-hook
  "Default implementation of *throw-hook*. If object in context is a
  Throwable, throws it, else wraps it and throws the wrapper."
  [{:keys [object cause stack-trace] :as context}]
  (throw
   (if (instance? Throwable object)
     object
     (make-throwable (context-message context) cause stack-trace context))))

(defn make-stack-trace
  "Returns the current stack trace beginning at the caller's frame"
  []
  (let [trace (.getStackTrace (Thread/currentThread))]
    (java.util.Arrays/copyOfRange trace 2 (alength trace))))
