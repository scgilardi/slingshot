(ns slingshot.core
  (:use [slingshot.Exception]))

(defn- catch-form? [x]
  (and (seq? x) (= 'catch (first x))))

(defn- type-name? [x]
  (or (keyword? x)
      (and (symbol? x) (class? (resolve x)))))

(defmacro throw+ [obj]
  `(throw (slingshot.Exception. ~obj (zipmap '~(keys &env) [~@(keys &env)]))))

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
                 (hash-map
                  :env (-> throwable# .state :env)
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
