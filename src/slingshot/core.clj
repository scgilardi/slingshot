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

(defn context
  "Returns the context map associated with t. Works around CLJ-292."
  [t]
  (loop [c t]
    (cond (instance? Stone c)
          (.context c)
          (= RuntimeException (class c))
          (recur (.getCause c))
          :else
          {:obj t})))

(defmacro throw+
  "Like the throw special form, but can throw any object. Identical to
  throw for Throwable objects. For other objects, an optional second
  argument specifies a message displayed along with the object's value
  if it is caught outside a try+ form. Within a try+ catch clause,
  throw+ with no arguments rethrows the caught object.

  See also try+"
  ([obj msg]
     `(let [obj# ~obj]
        (throw
         (if (instance? Throwable obj#)
           obj#
           (let [env# (zipmap '~(keys &env) [~@(keys &env)])]
             (Stone.
              ~msg
              obj#
              {:obj obj#
               :env (dissoc env# '~'&throw-context)
               :next (env# '~'&throw-context)}))))))
  ([obj] `(throw+ ~obj "Object thrown by throw+:"))
  ([] `(throw (-> ~'&throw-context meta :throwable))))

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
       ~@(when catch-clauses
           `((catch Throwable ~'&throw-context
               (let [~'&throw-context
                     (-> (context ~'&throw-context)
                         (assoc :stack (.getStackTrace ~'&throw-context))
                         (with-meta {:throwable ~'&throw-context}))]
                 (cond
                  ~@(mapcat catch->cond catch-clauses)
                  :else
                  (throw+))))))
       ~@finally-clause)))
