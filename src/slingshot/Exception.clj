(ns slingshot.Exception
  (:gen-class :extends RuntimeException
              :state state
              :init init
              :constructors {[Object Object]
                             []}))

(defn -init
  [obj env]
  [[] {:obj obj :env env}])

(defn -toString [self]
  (str (.getCanonicalName (class self)) ": " (-> self .state :obj)))
