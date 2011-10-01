(ns slingshot.core
  (:import (slingshot Stone)))

(defn- clause-type [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn- partition-body [body]
  (let [[e c f s] (partition-by clause-type body)
        [e c f s] (if (-> (first e) clause-type nil?) [e c f s] [nil e c f])
        [c f s] (if (-> (first c) clause-type (= 'catch)) [c f s] [nil c f])
        [f s] (if (-> (first f) clause-type (= 'finally)) [f s] [nil f])]
    (when (or s (> (count f) 1))
      (throw (Exception. (str "try+ form must match: "
                              "(try+ expr* catch-clause* finally-clause?)"))))
    [e c f]))

(defn- classname? [x]
  (and (symbol? x) (class? (resolve x))))

(defn- catch->cond [[_ selector binding-form & exprs]]
  [(cond (classname? selector)
         `(instance? ~selector (:obj ~'&throw-context))
         (seq? selector)
         (clojure.walk/prewalk-replace
          {(-> *ns* ns-name name (symbol "%")) '(:obj &throw-context)}
          selector)
         :else
         `(~selector (:obj ~'&throw-context)))
   `(let [~binding-form (:obj ~'&throw-context)]
      ~@exprs)])

(defn- transform
  "Transform try+ catch-clauses and default into a try-compatible catch"
  [catch-clauses default]
  ;; the code below uses only one local to minimize clutter in the
  ;; &env captured by throw+ forms within catch clauses (see the
  ;; special handling of &throw-context in throw+)
  `(catch Throwable ~'&throw-context
     (let [~'&throw-context (*catch-hook* (context ~'&throw-context))]
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
  (drop 2 (.getStackTrace (Thread/currentThread))))

(defn make-throwable
  "Given a context from throw+, returns a Throwable to be thrown"
  [msg {:keys [obj] :as context}]
  (if (instance? Throwable obj)
    obj
    (Stone. msg obj context)))

(defn default-throw-hook
  "Default implementation of *throw-hook*. Makes a throwable from a
  context and throws it."
  [{:keys [msg context]}]
  (throw (make-throwable msg context)))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a map with keys :msg
  and :context. defaults to default-throw-hook"}
  *throw-hook* default-throw-hook)

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of catch. Must be
  bound to a function of one argument, a context map with
  metadata. Returns a (possibly modified) context map to be considered
  by catch clauses. Existing metadata on the argument map must be
  preserved (or modified intentionally) in the returned context map.

  Normal catch processing can be overridden by adding special keys to
  the returned metadata:

    - if the metadata includes the key :catch-hook-return, try+ will
      return the corresponding value; else

    - if the metadata includes the key :catch-hook-throw, try+ will throw
      the corresponding value

  defaults to identity"}
  *catch-hook* identity)

(defn context
  "Returns the context map for Throwable t."
  [t]
  ;; unwrapping RuntimeException cause chains works around CLJ-292.
  (-> (loop [c t]
        (cond (instance? Stone c)
              (.context c)
              (= RuntimeException (class c))
              (recur (.getCause c))
              :else
              {:obj t}))
      (assoc :stack (.getStackTrace t))
      (with-meta {:throwable t})))

(defmacro throw+
  "Like the throw special form, but can throw any object. Identical to
  throw for Throwable objects. For other objects, an optional second
  argument specifies a message displayed along with the object's value
  if it is caught outside a try+ form. Within a try+ catch clause,
  throw+ with no arguments rethrows the caught object.

  See also try+"
  ([obj msg]
     `(*throw-hook*
       (let [env# (zipmap '~(keys &env) [~@(keys &env)])]
         {:msg ~msg
          :context {:obj ~obj
                    :stack (make-stack-trace)
                    :env (dissoc env# '~'&throw-context)
                    :next (env# '~'&throw-context)}})))
  ([obj] `(throw+ ~obj "Object thrown by throw+:"))
  ([] `(throw (-> ~'&throw-context meta :throwable))))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses:
    - specify objects to catch by classname, predicate, or
      selector form;
    - destructure the caught object;
    - access the dynamic context at the throw site via the
      &throw-context hidden argument.

  A selector form is a form containing one or more instances of % to
  be replaced by the thrown object. If it evaluates to truthy, the
  object is caught.

  Classname and predicate selectors are shorthand for these selector
  forms:

    <classname> => (instance? <classname> %)
    <predicate> => (<predicate> %)

  &throw-context is a map containing:
    :obj the thrown object;
    :stack the stack trace;
  for all caught objects, and
    :env a map of bound symbols to their values;
    :next the next throw context in the cause chain
  for objects that are not instances of Throwable.

  See also throw+"
  [& body]
  (let [[exprs catch-clauses finally-clause] (partition-body body)]
    `(try
       ~@exprs
       ~@(when catch-clauses
           [(transform catch-clauses '(throw+))])
       ~@finally-clause)))
