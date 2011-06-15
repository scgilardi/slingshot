(ns slingshot.core
  (:require [slingshot.Exception]))

(defrecord context [obj env next])

(defn- clause? [x]
  (and (seq? x) (#{'catch 'finally} (first x))))

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
  (let [[try-body catch-clauses finally-clause] (partition-by clause? body)
        thrown (gensym)]
    `(try
       ~@try-body
       (catch Throwable throwable#
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
            (throw throwable#))))
       ~@finally-clause)))
