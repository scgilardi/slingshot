(ns slingshot.core
  (:require [slingshot.Exception]))

(defrecord stone [obj env next])

(defn- clause? [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn- finally? [x]
  (when (seq? x) (#{'finally} (first x))))

(defn- class-name? [x]
  (and (symbol? x) (class? (resolve x))))

(defn- type-spec? [x]
  (and (map? x) (= 1 (count x))))

(defn throw-context [throwable]
  (when (instance? slingshot.Exception throwable)
    (assoc (.state throwable)
      :stack (->> throwable .getStackTrace (drop 3) into-array))))

(defn thrown [throwable]
  (if (instance? slingshot.Exception throwable)
    (-> throwable .state :obj)
    throwable))

(defmacro throw+
  [obj & [cause-context]]
  `(throw (slingshot.Exception.
           (stone. ~obj (zipmap '~(keys &env) [~@(keys &env)])
                   ~cause-context))))

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
               (let [~thrown (thrown throwable#)
                     ~'&throw-context (throw-context throwable#)]
                 (cond
                  ~@(mapcat
                     (fn [[_ selector local-name & catch-body]]
                       [(cond (class-name? selector)
                              `(instance? ~selector ~thrown)
                              (type-spec? selector)
                              (let [[hierarchy parent] (first (seq selector))]
                                (if (nil? hierarchy)
                                  `(isa? (type ~thrown) ~parent)
                                  `(isa? ~hierarchy (type ~thrown) ~parent)))
                              :else
                              `(~selector ~thrown))
                        `(let [~local-name ~thrown]
                           ~@catch-body)])
                     catch-clauses)
                  :else
                  (throw throwable#))))])
       ~@finally-clause)))
