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

(defn- typespec? [x]
  (and (map? x) (= 1 (count x))))

(defn- catch->cond [[_ selector binding-form & exprs]]
  [(cond (classname? selector)
         `(instance? ~selector (:obj ~'&throw-context))
         (typespec? selector)
         (let [[hierarchy parent] (first selector)]
           (if (nil? hierarchy)
             `(isa? (type (:obj ~'&throw-context)) ~parent)
             `(isa? ~hierarchy (type (:obj ~'&throw-context)) ~parent)))
         :else
         `(~selector (:obj ~'&throw-context)))
   `(let [~binding-form (:obj ~'&throw-context)]
      ~@exprs)])

(defn make-stack-trace
  "Returns the current stack trace beginning at the caller's frame"
  []
  (drop 2 (.getStackTrace (Thread/currentThread))))

(defn make-throwable
  "Given a context from throw+, returns a Throwable to be thrown"
  [{:keys [obj] :as context}]
  (if (instance? Throwable obj)
    obj
    (Stone.
     "Object thrown by throw+ not caught in any try+:"
     obj
     context)))

(defn default-throw-hook
  "Default implementation of *throw-hook*. Makes a throwable from a
  context and throws it."
  [context]
  (throw (make-throwable context)))

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of throw+. Must be
  bound to a function of one argument, a context map. defaults to
  default-throw-hook"}
  *throw-hook* default-throw-hook)

(def ^{:dynamic true
       :doc "Hook to allow overriding the behavior of catch. Must be
  bound to a function of one argument, a context map. returns a
  (possibly modified) context map to be considered by catch clauses or
  nil to disable further catch processing. defaults to identity"}
  *catch-hook* identity)

(defmacro throw+
  "Like the throw special form, but can throw any object.
  See also try+"
  [obj]
  `(*throw-hook*
    (let [env# (zipmap '~(keys &env) [~@(keys &env)])]
      {:obj ~obj
       :stack (make-stack-trace)
       :env (dissoc env# '~'&throw-context)
       :next (env# '~'&throw-context)})))

(defmacro try+
  "Like the try special form, but with enhanced catch clauses:
    - specify objects to catch by classname, predicate, or typespec;
    - destructure the caught object;
    - access the dynamic context at the throw site via the
      &throw-context hidden argument.

  A typespec is a map with one entry:
    - the key is the hierarchy (or nil for the global hierarchy);
    - the value is the type tag: a keyword or symbol.

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
       ~@finally-clause)))
       (catch Throwable ~'&throw-context
         ;; written carefully to introduce only one symbol into
         ;; into the environment that's visible from within
         ;; throw+ forms in catch clauses (see the special
         ;; handling of &throw-context in throw+)
         (let [~'&throw-context
               (-> (if (instance? Stone ~'&throw-context)
                     (.context ~'&throw-context)
                     {:obj ~'&throw-context
                      :stack (.getStackTrace ~'&throw-context)})
                   (with-meta {:throwable ~'&throw-context})
                   (*catch-hook*))]
           (when ~'&throw-context
             (cond
              ~@(mapcat catch->cond catch-clauses)
              :else
              (throw (-> ~'&throw-context meta :throwable))))))
