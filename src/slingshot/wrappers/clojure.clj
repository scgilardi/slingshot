(ns slingshot.wrappers.clojure
  (import clojure.lang.ExceptionInfo))

(defn wrap [data message cause stack-trace]
  (doto (ExceptionInfo. message (vary-meta data assoc ::wrapper? true) cause)
    (.setStackTrace stack-trace)))

(defn unwrap [t]
  (when (instance? ExceptionInfo t)
    (let [data (.getData ^ExceptionInfo t)]
      (when (::wrapper? (meta data))
        (vary-meta data dissoc ::wrapper?)))))
