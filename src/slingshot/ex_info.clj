(ns slingshot.ex-info
  "provides implementations of ex-info and ex-data for slingshot to
  use with clojure versions earlier than 1.4.0

  ex-info and ex-data are currently scheduled to be available in
  clojure.core starting with release 1.4.0. These implementations are
  based on that code (issue CLJ-733), using slingshot.ExceptionInfo in
  place of clojure.lang.ExceptionInfo.

  This allows slingshot to be compatible with previous versions of
  clojure while also fully supporting clojure.1.4.0 and whatever tools
  may be created based on clojure.lang.ExceptionInfo."
  (import slingshot.ExceptionInfo))

(defn ex-info
  "Create an instance of ExceptionInfo, a RuntimeException subclass
  that carries a map of additional data."
  [msg map cause]
  (ExceptionInfo. msg map cause))

(defn ex-data
  "Returns exception data (a map) if ex is an ExceptionInfo.
  Otherwise returns nil."
  [ex]
  (when (instance? ExceptionInfo ex)
    (.getData ^ExceptionInfo ex)))
