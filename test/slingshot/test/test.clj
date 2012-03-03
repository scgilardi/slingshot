(ns slingshot.test.test
  (:use [clojure.test]
        [slingshot.slingshot :only [throw+]])
  (:require [slingshot.test]))

(deftest test-slingshot-test-macros
  (is (thrown+? string? (throw+ "test")))
  (is (thrown+-with-msg? string? #"th" (throw+ "test" "hi there"))))
