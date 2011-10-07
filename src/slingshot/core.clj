(ns slingshot.core
  (:use [clojure.walk :only [prewalk-replace]])
  (:import (slingshot Stone)))

(defn- clause-type
  "Returns a classifying value for any object in a try+ body:
  catch-clause, finally-clause, or other"
  [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn- partition-body
  "Partitions a try+ body into exprs, catch-clauses, and finally
  clause where each partition may be empty. Throws if the body doesn't
  match (expr* catch-clause* finally-clause?)"
  [body]
  (let [[e c f s] (partition-by clause-type body)
        [e c f s] (if (-> (first e) clause-type nil?) [e c f s] [nil e c f])
        [c f s] (if (-> (first c) clause-type (= 'catch)) [c f s] [nil c f])
        [f s] (if (-> (first f) clause-type (= 'finally)) [f s] [nil f])]
    (when (or s (> (count f) 1))
      (throw (IllegalArgumentException.
              (str "try+ form must match: "
                   "(try+ expr* catch-clause* finally-clause?)"))))
    [e c f]))

(defn- resolved
  "For symbols, returns the resolved value or throws if not resolvable"
  [x]
  (when (symbol? x)
    (or (resolve x)
        (throw (IllegalArgumentException.
                (str "Unable to resolve symbol: " x " in this context"))))))

(defn- ns-qualify
  "Returns a fully qualified symbol whose namespace is the current
  namespace and whose name the same name as the name of sym"
  [sym]
  (-> *ns* ns-name name (symbol (name sym))))

(defn- sep-pr-str
  "Helper for the throw+ syntax error message"
  [x]
  (str " " (pr-str x)))

(defn- catch->cond
  "Converts a try+ catch clause into a test/expr pair for cond"
  [[_ selector binding-form & exprs]]
  [(cond (class? (resolved selector))
         `(instance? ~selector (:object ~'&throw-context))
         (seq? selector)
         (prewalk-replace {(ns-qualify '%) '(:object &throw-context)} selector)
         :else
         `(~selector (:object ~'&throw-context)))
   `(let [~binding-form (:object ~'&throw-context)]
      ~@exprs)])

(defn- transform
  "Transforms a seq of try+ catch-clauses and default into a single
  try-compatible catch"
  [catch-clauses default]
  ;; the code below uses only one local to minimize clutter in the
  ;; &env captured by throw+ forms within catch clauses (see the
  ;; special handling of &throw-context in throw+)
  `(catch Throwable ~'&throw-context
     (let [~'&throw-context (-> ~'&throw-context throw-context *catch-hook*)]
       (cond
        (contains? (meta ~'&throw-context) :catch-hook-return)
        (:catch-hook-return (meta ~'&throw-context))
        (contains? (meta ~'&throw-context) :catch-hook-throw)
        (throw+ (:catch-hook-throw (meta ~'&throw-context)))
        ~@(mapcat catch->cond catch-clauses)
        :else ~default))))

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
  (str (or message "Object thrown by throw+") ": " (pr-str object)))

(defn default-throw-hook
  "Default implementation of *throw-hook*. If object in context is a
  Throwable, throws it, else wraps it and throws the wrapper."
  [{:keys [object cause stack-trace] :as context}]
  (throw
   (if (instance? Throwable object)
     object
     (make-throwable (context-message context) cause stack-trace context))))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. Defaults to
  default-throw-hook."}
  *throw-hook* default-throw-hook)

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

(defmacro throw+
  "Like the throw special form, but can throw any object. Behaves the
  same as throw for Throwable objects. For other objects, an optional
  second argument specifies a message that is accessible in catch
  clauses within both try forms (via .getMessage on the throwable
  wrapper), and try+ forms (via the :message key in &throw-context).
  Within a try+ catch clause, throw+ with no arguments rethrows the
  caught object within its original (possibly nested) wrappers.

  See also try+"
  ([object & [message & sentinel]]
     (when (or sentinel (and message (not (string? message))))
       (throw (IllegalArgumentException.
               (format "throw+ call must match: (throw+ %s): (throw+ %s)"
                       "object? ^String message?"
                       (apply str
                              (pr-str object)
                              (and message (sep-pr-str message))
                              (and sentinel (mapcat sep-pr-str sentinel)))))))
     `(*throw-hook*
       (let [env# (zipmap '~(keys &env) [~@(keys &env)])]
         {:object ~object
          :message ~message
          :cause (-> (env# '~'&throw-context) meta :throwable)
          :stack-trace (make-stack-trace)
          :environment (dissoc env# '~'&throw-context)})))
  ([] `(throw (-> ~'&throw-context meta :throwable))))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses:
    - specify objects to catch by classname, predicate, or
      selector form;
    - destructure the caught object;
    - access the values of the locals visible at the throw site via
      the &throw-context hidden argument.

  A selector form is a form containing one or more instances of % to
  be replaced by the thrown object. If it evaluates to truthy, the
  object is caught.

  The classname and predicate selectors are shorthand for these
  selector forms:

    <classname> => (instance? <classname> %)
    <predicate> => (<predicate> %)

  &throw-context is a map containing:
    - for all caught objects:
      :object       the thrown object;
      :stack-trace  the stack trace;
    - for Throwable caught objects:
      :message      the message, from .getMessage;
      :cause        the cause, from .getCause;
    - for non-Throwable caught objects:
      :message      the message, from the optional argument to throw+;
      :cause        the cause, captured by throw+, see below;
      :wrapper      the outermost Throwable wrapper of the caught object,
                    see below;
      :environment  a map of locals visible at the throw+ site: symbols
                    mapped to their bound values.

  To throw a non-Throwable object, throw+ wraps it with an Throwable
  object of class Stone. That Stone may in turn end up wrapped by
  other exceptions (e.g., instances of RuntimeException or
  java.util.concurrent.ExecutionException). try+ \"sees through\" all
  such wrappers to find the object wrapped by the first instance of
  Stone in the outermost wrapper's cause chain. If needed, the
  outermost wrapper is available within a catch clause a via
  the :wrapper key in &throw-context. Any nested wrappers are
  accessible via its cause chain.

  When throw+ throws a non-Throwable object from within a try+ catch
  clause, the outermost wrapper of the caught object being processed
  is captured as the \"cause\" of the new throw+.

  See also throw+"
  [& body]
  (let [[exprs catch-clauses finally-clause] (partition-body body)]
    `(try
       ~@exprs
       ~@(when catch-clauses
           [(transform catch-clauses '(throw+))])
       ~@finally-clause)))
