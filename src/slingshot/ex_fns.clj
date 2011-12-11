(ns slingshot.ex-fns
  (import slingshot.ExceptionInfo))

;; copied from clojure 1.4, but using slingshot.ExceptionInfo for
;; compatibility with earlier clojure versions

(defn ex-data
  "Returns exception data (a map) if ex is an ExceptionInfo.
  Otherwise returns nil."
  [ex]
  (when (instance? ExceptionInfo ex)
    (.getData ^slingshot.ExceptionInfo ex)))

(defn ex-info
  "Create an instance of ExceptionInfo, a RuntimeException subclass
  that carries a map of additional data."
  [msg map cause]
  (ExceptionInfo. msg map cause))
