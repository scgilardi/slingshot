(ns slingshot.stone)

(defprotocol Stone (data [this]))

(defn create [data]
  (let [stone (proxy [RuntimeException slingshot.stone.Stone] []
                (data [] data))]
    (->> (.getStackTrace stone) (drop 6) into-array (.setStackTrace stone))
    stone))
