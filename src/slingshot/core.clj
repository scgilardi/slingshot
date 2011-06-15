(ns slingshot.core
  (:require [slingshot.Exception]))

(defrecord context [obj env next])

(defn- clause? [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn- finally? [x]
  (when (seq? x) (#{'finally} (first x))))

(defn- type-name? [x]
  (or (keyword? x)
      (and (symbol? x) (class? (resolve x)))))

(defmacro throw+
  ([obj]
     `(throw (slingshot.Exception.
              (context. ~obj (zipmap '~(keys &env) [~@(keys &env)])
                        nil))))
  ([obj cause-context]
     `(throw (slingshot.Exception.
              (context. ~obj (zipmap '~(keys &env) [~@(keys &env)])
                        cause-context)))))

(defmacro try+
  [& body]
  (let [[b c f] (partition-by clause? body)
        [b c f] (if (clause? (first b))
                  [nil b c]
                  [b c f])
        [c f] (if (finally? (first c))
                [nil c]
                [c f])
        [try-body catch-clauses finally-clause] [b c f]
        thrown (gensym)]
    `(try
       ~@try-body
       ~@(when catch-clauses
           `[(catch Throwable throwable#
               (let [~thrown (if (instance? slingshot.Exception throwable#)
                               (-> throwable# .state :obj)
                               throwable#)
                     ~'&throw-context
                     (when (instance? slingshot.Exception throwable#)
                       (assoc (-> throwable# .state)
                         :stack (into-array (drop 3 (.getStackTrace throwable#)))))]
                 (cond
                  ~@(mapcat
                     (fn [[_ type-or-pred local-name & catch-body]]
                       [(if (type-name? type-or-pred)
                          `(isa? (type ~thrown) ~type-or-pred)
                          `(~type-or-pred ~thrown))
                        `(let [~local-name ~thrown]
                           ~@catch-body)])
                     catch-clauses)
                  :else
                  (throw throwable#))))])
       ~@finally-clause)))
