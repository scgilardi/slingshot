(ns slingshot.util
  (:require [clojure.walk :refer [postwalk]]))

(defn cljs-env?
  "Given an &env from a macro, tells whether it is expanding into CLJS."
  [env]
  (boolean (:ns env)))

#?(:clj
(defmacro if-cljs
  "Return @then if the macro is generating CLJS code and @else for CLJ code."
  {:from "https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"}
  ([env then else]
    `(if (cljs-env? ~env) ~then ~else))))

#?(:clj
(defmacro when-cljs
  "Return @then if the macro is generating CLJS code."
  ([env then]
    `(when (cljs-env? ~env) ~then))))

(defn appears-within?
  "Returns true if x appears within coll at any nesting depth"
  [x coll]
  (let [result (atom false)]
    (postwalk
      (fn [t]
        (when (= x t)
          (reset! result true)))
     coll)
    @result))