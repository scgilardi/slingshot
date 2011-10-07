(ns slingshot.test.hooks
  (:use [clojure.test]
        [slingshot.core :only [try+ throw+]]
        [slingshot.hooks :only [*throw-hook* *catch-hook*]]))

(def test-hooked (atom nil))

(deftest test-throw-hook
  (binding [*throw-hook* #(reset! test-hooked %)]
    (throw+ "throw-hook-string")
    (is (= (set (keys @test-hooked))
           (set [:object :message :cause :stack-trace :environment])))
    (is (= "throw-hook-string" (:object @test-hooked))))
  (binding [*throw-hook* (fn [x] 42)]
    (is (= (throw+ "something") 42))))

(def catch-hooked (atom nil))

(defn catch-hook-return [object]
  (fn [x] (vary-meta x assoc :catch-hook-return object)))

(defn catch-hook-throw [object]
  (fn [x] (vary-meta x assoc :catch-hook-throw object)))

(deftest test-catch-hook
  (binding [*catch-hook* #(reset! catch-hooked %)]
    (try+ (throw+ "catch-hook-string") (catch string? x x))
    (is (= (set (keys @catch-hooked))
           (set [:wrapper :object :message :cause :stack-trace :environment])))
    (is (= "catch-hook-string" (:object @catch-hooked))))
  (binding [*catch-hook* (catch-hook-return 42)]
    (is (= 42 (try+ (throw+ "boo") (catch string? x x)))))
  (binding [*catch-hook* (catch-hook-throw (IllegalArgumentException. "bleh"))]
    (is (thrown-with-msg? IllegalArgumentException #"bleh"
          (try+ (throw+ "boo") (catch string? x x)))))
  (is (= "soup!"
         (try+
          (binding [*catch-hook* (catch-hook-throw "soup!")]
            (try+
             (throw+ "boo")
             (catch string? x x)))
          (catch string? x x)))))
