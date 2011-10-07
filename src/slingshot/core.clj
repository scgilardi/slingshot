(ns slingshot.core
  (:use [clojure.walk :only [prewalk-replace]])
  (:import (slingshot Stone)))

(defn- clause-type
  "Return a classifying value for any object in a try+ body:
  catch-clause, finally-clause, or other"
  [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn- partition-body
  "Partition and syntax check a try+ body"
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
  "For symbols, return the resolved value or throw if not resolvable"
  [x]
  (when (symbol? x)
    (or (resolve x)
        (throw (IllegalArgumentException.
                (str "Unable to resolve symbol: " x " in this context"))))))

(defn- ns-qualify
  "Return a fully qualified symbol with the current namespace and the
  same name as sym"
  [sym]
  (-> *ns* ns-name name (symbol (name sym))))

(defn- catch->cond
  "Convert a try+ catch clause into the two parts of a cond clause"
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
  "Transform try+ catch-clauses and default into a try-compatible catch"
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
        (throw (:catch-hook-throw (meta ~'&throw-context)))
        ~@(mapcat catch->cond catch-clauses)
        :else ~default))))

(defn make-stack-trace
  "Returns the current stack trace beginning at the caller's frame"
  []
  (let [trace (.getStackTrace (Thread/currentThread))]
    (java.util.Arrays/copyOfRange trace 2 (alength trace))))

(defn make-throwable
  "Make a message, cause, stack-trace, and context throwable by wrapping"
  [message cause stack-trace context]
  (Stone. message cause stack-trace context))

(defn context-message
  "Return a message string given a context"
  [{:keys [message object]}]
  (str (or message "Object thrown by throw+") ": " (pr-str object)))

(defn default-throw-hook
  "Default implementation of *throw-hook*. If object in context is a
  Throwable, throw it, else wrap it and throw the wrapper."
  [{:keys [object cause stack-trace] :as context}]
  (throw
   (if (instance? Throwable object)
     object
     (make-throwable (context-message context) cause stack-trace context))))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. Defaults to
  default-throw-hook"}
  *throw-hook* default-throw-hook)

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of catch. Must be
  bound to a function of one argument, a context map with
  metadata. Returns a (possibly modified) context map to be considered
  by catch clauses. Existing metadata on the context map must be
  preserved (or intentionally modified) in the returned context map.

  Normal processing by catch clauses can be preempted by adding
  special keys to the metadata on the returned context map:

  If the metadta contains the key:
    - :catch-hook-return, try+ will return the corresponding value;
    - :catch-hook-throw, try+ will throw the corresponding value.

  Defaults to identity."}
  *catch-hook* identity)

(defn throw-context
  "Returns the context map associated with t. If t or any throwable in
  its cause chain is a Stone, return its context, else return a new
  context with t as the thrown object."
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
  second argument specifies a message which by default is displayed
  along with the object's value if it is caught outside a try+
  form. Within a try+ catch clause, throw+ with no arguments rethrows
  the caught object.

  See also try+"
  ([object & [message sen]]
     (when sen
       (throw (IllegalArgumentException.
               "throw+ call must match: (throw+ object? ^String message?")))
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
    - access the context at the throw site via the &throw-context
      hidden argument.

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
      :environment  a map of bound symbols to their values.

  To throw a non-Throwable object, throw+ wraps it with an object of
  type Stone. That Stone in turn may end up being wrapped by other
  exceptions (e.g., instances of RuntimeException or
  java.util.concurrent.ExecutionException). try+ \"sees through\" all
  such wrappers to find the object wrapped by the first instance of
  Stone in the outermost wrapper's cause chain. If needed, the
  outermost wrapper is available within a catch clause at the :wrapper
  key in &throw-context. Any nested wrappers are accessible via its
  cause chain.

  When throw+ throws a non-Throwable object from within a try+ catch
  clause, the outermost wrapper of the caught object being processed
  is captured as the \"cause\" of the new throw.

  See also throw+"
  [& body]
  (let [[exprs catch-clauses finally-clause] (partition-body body)]
    `(try
       ~@exprs
       ~@(when catch-clauses
           [(transform catch-clauses '(throw+))])
       ~@finally-clause)))
