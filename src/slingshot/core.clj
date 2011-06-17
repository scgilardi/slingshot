(ns slingshot.core)

(defn- clause? [x]
  (when (seq? x) (#{'catch 'finally} (first x))))

(defn- finally? [x]
  (when (seq? x) (#{'finally} (first x))))

(defn- class-name? [x]
  (and (symbol? x) (class? (resolve x))))

(defn- type-spec? [x]
  (and (map? x) (= 1 (count x))))

(defn- partition-body [body]
  (let [[b c f] (partition-by clause? body)
        [b c f] (if (clause? (first b)) [nil b c] [b c f])
        [c f] (if (finally? (first c)) [nil c] [c f])]
    [b c f]))

(defn- cond-clause [selector local-name thrown catch-body]
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

(defn thrown [throwable]
  (if (instance? slingshot.Exception throwable)
    (-> throwable .state :obj)
    throwable))

(defn throw-context [throwable]
  (when (instance? slingshot.Exception throwable)
    (assoc (.state throwable)
      :stack (->> throwable .getStackTrace (drop 5) into-array))))

(defmacro throw+
  "Like the throw special form, but can throw any object.
  See also try+"
  [obj]
  `(let [env# (zipmap '~(keys &env) [~@(keys &env)])]
     (throw (slingshot.Exception.
             {:obj ~obj
              :env (dissoc env# '~'&throw-context '~'&throwable)
              :next (env# '~'&throw-context)}))))

(defmacro try+
  "Like the try special form, but supports enhanced catch clauses:
  select thrown object by class, predicate, or type specifier;
  destructure the caught object; retrieve the dynamic context at the
  throw site via the &throw-context hidden argument. A type-specifier
  is a map with one entry: the key is the hierarchy (or nil for the
  global hierarchy), and the value is the type tag. &throw-context
  provides values for keys: :obj (the caught object), :env (a map of
  bound symbols to their values), :stack (the stack trace), :next (the
  next context in the cause chain, or nil for a root cause).
  See also throw+"
  [& body]
  (let [[try-body catch-clauses finally-clause] (partition-body body)]
    `(try
       ~@try-body
       ~@(when catch-clauses
           `((catch Throwable ~'&throwable
               (let [~'&throw-context (throw-context ~'&throwable)]
                 (cond
                  ~@(mapcat
                     (fn [[_ selector local-name & catch-body]]
                       (cond-clause
                        selector local-name `(thrown ~'&throwable) catch-body))
                     catch-clauses)
                  :else
                  (throw ~'&throwable))))))
       ~@finally-clause)))
