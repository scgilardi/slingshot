(ns slingshot.test.core
  (:use [clojure.test]
        [slingshot.core :only [try+ throw+]]))

(defrecord oit-exception [error-code duration-ms message])
(defrecord x-failure [message])

(def exception-1 (Exception. "exceptional"))
(def oit-exception-1 (oit-exception. 6 1000 "pdf failure"))

(defn mult-func [x y]
  (let [a 7 b 11]
    (if (= x 3)
      (* a b x y)
      (throw+ (x-failure. "x isn't 3... really??")))))

(defn test-func [x y]
  (try+
   (mult-func x y)
   (catch x-failure {msg :message}
     [msg (:env &throw-context)])))

(defmacro mega-try [body]
  `(try+
    ~body
    (catch nil? e#
      [:nil e#])
    (catch Integer e#
      [:integer e#])
    (catch keyword? e#
      [:keyword e#])
    (catch symbol? e#
      [:symbol e#])
    (catch {nil :oit-exception} e#
      [:oit-exception-map e#])
    (catch oit-exception e#
      [:oit-exception-record e#])
    (catch map? e#
      [:map e# (meta e#)])
    (catch IllegalArgumentException e#
      [:iae e#])
    (catch Exception e#
      [:exception e#])))

(deftest test-try+
  (testing "throwing scalar types (to demonstrate genericity)"
    (is (= [:nil nil] (mega-try (throw+ nil))))
    (is (= [:integer 4] (mega-try (throw+ 4))))
    (is (= [:keyword :awesome] (mega-try (throw+ :awesome))))
    (is (= [:symbol 'yuletide] (mega-try (throw+ 'yuletide)))))
  (testing "wrapped exception"
    (is (= [:exception exception-1] (mega-try (throw+ exception-1)))))
  (testing "unwrapped exception (interop with normal throw)"
    (is (= [:exception exception-1] (mega-try (throw exception-1)))))
  (testing "catching a map by predicate"
    (is (= [:map {:error-code 4} nil] (mega-try (throw+ {:error-code 4})))))
  (testing "catching a map with metadata by predicate"
    (is (= [:map {:error-code 4} {:severity 4}]
           (mega-try (throw+ ^{:severity 4} {:error-code 4})))))
  (testing "catching a map with :type metadata by type"
    (is (= [:oit-exception-map {:error-code 5}]
           (mega-try (throw+ ^{:type :oit-exception} {:error-code 5})))))
  (testing "catching a record acting as a custom exception type"
    (is (= [:oit-exception-record oit-exception-1]
           (mega-try (throw+ (oit-exception. 6 1000 "pdf failure"))))))
  (testing "catching an organic IllegalArgumentException"
    (is (= :iae (first (mega-try (first 1)))))))

(deftest test-locals-and-destructuring
  (is (= 1155 (test-func 3 5)))
  (is (= ["x isn't 3... really??"
          {'mult-func mult-func 'x 4 'y 7 'a 7 'b 11}] (test-func 4 7))))

(deftest test-clauses
  (let [bumps (atom 0)
        bump (fn [] (swap! bumps inc))]
    (is (nil? (try+)))
    (is (nil? (try+ (catch Integer i 4))))
    (is (nil? (try+ (finally (bump)))))
    (is (nil? (try+ (catch Integer i 4) (finally (bump)))))
    (is (= 3 (try+ 3)))
    (is (= 3 (try+ 3 (catch Integer i 4))))
    (is (= 3 (try+ 3 (finally (bump)))))
    (is (= 3 (try+ 3 (catch Integer i 4) (finally (bump)))))
    (is (= 4 (try+ (throw+ 3) (catch Integer i (inc i)) (finally (bump)))))
    (is (= 4 (try+ 3 4)))
    (is (= 4 (try+ 3 4 (catch Integer i 4))))
    (is (= 4 (try+ 3 4 (finally (bump)))))
    (is (= 4 (try+ 3 4 (catch Integer i 4) (finally (bump)))))
    (is (= 5 (try+ (throw+ 4) 4 (catch Integer i (inc i)) (finally (bump)))))
    (is (= 8 @bumps))))
