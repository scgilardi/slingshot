(ns slingshot.test-test
  (:require [#?(:clj  clojure.test
                :cljs cljs.test)
              :refer [#?@(:clj [deftest is])]
              :refer-macros [deftest is]]
            [slingshot.slingshot
              :refer        [#?(:clj throw+)]
              :refer-macros [throw+]]
            [slingshot.test]))

#?(:clj
(deftest test-slingshot-test-macros ; FIXME Doesn't work in CLJS because cljs.test/assert-expr multimethod is CLJ only
  (is (thrown+? string? (throw+ "test")))
  (is (thrown+-with-msg? string? #"th" (throw+ "test" "hi there")))))