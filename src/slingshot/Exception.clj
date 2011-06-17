(ns slingshot.Exception
  (:gen-class :extends RuntimeException
              :state state
              :init init
              :post-init post-init
              :constructors {[Object] []}))

(defn -init [obj]
  [[] obj])

(defn -post-init [self obj]
  (->> (.getStackTrace self) (drop 5) into-array (.setStackTrace self)))
