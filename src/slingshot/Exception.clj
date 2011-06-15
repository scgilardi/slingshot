(ns slingshot.Exception
  (:gen-class :extends RuntimeException
              :state state
              :init init
              :constructors {[Object] []}))

(defn -init [obj]
  [[] obj])
