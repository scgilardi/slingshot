(ns slingshot.wrappers.slingshot
  (:import slingshot.ExceptionInfo))

(defn wrap [data message cause stack-trace]
  (doto (ExceptionInfo. message (with-meta data {:type ::wrapper}) cause)
    (.setStackTrace stack-trace)))

(defn unwrap [t]
  (when (instance? ExceptionInfo t)
    (let [data (.getData ^ExceptionInfo t)]
      (when (= (type data) ::wrapper)
        data))))
