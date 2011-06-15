(ns slingshot.core
  (:use [slingshot.Exception]))

(defrecord context [obj env next])

(defn- catch-form? [x]
  (and (seq? x) (= 'catch (first x))))

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
  (let [catch-clauses (filter catch-form? body)
        try-body (remove catch-form? body)
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
            (throw throwable#)))))))
