(ns slingshot.test.core
  (:use [clojure.test]
        [slingshot.core :only [try+ throw+]]))

(defrecord oit-exception [error-code duration-ms message])
(defrecord x-failure [message])

(def h1 (derive (make-hierarchy) ::square ::shape))
(def a-square ^{:type ::square} {:size 4})

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
    (catch {h1 ::shape} e#
      [:shape (type e#) e#])
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
  (testing "treat throwables exactly as throw does"
    (is (= [:exception exception-1]
           (mega-try (throw+ exception-1))
           (mega-try (throw exception-1))
           (try (throw+ exception-1) (catch Exception e [:exception e]))
           (try (throw exception-1) (catch Exception e [:exception e])))))
  (testing "catching an object by type in an ad-hoc hierarchy"
    (is (= [:shape ::square a-square] (mega-try (throw+ a-square)))))
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
    (is (nil? (try+ (catch Integer i (inc i)))))
    (is (nil? (try+ (finally (bump)))))
    (is (nil? (try+ (catch Integer i (inc i)) (finally (bump)))))
    (is (nil? (try+ (catch Integer i (inc i)) (catch map? m m)
                    (finally (bump)))))

    (is (= 3 (try+ 3)))
    (is (= 3 (try+ 3 (catch Integer i 4))))
    (is (= 3 (try+ 3 (finally (bump)))))
    (is (= 3 (try+ 3 (catch Integer i 4) (finally (bump)))))
    (is (= 4 (try+ (throw+ 3) (catch Integer i (inc i)) (finally (bump)))))
    (is (= 4 (try+ (throw+ 3) (catch Integer i (inc i)) (catch map? m m)
                   (finally (bump)))))
    (is (= 4 (try+ (throw+ {:sel 4}) (catch Integer i (inc i))
                   (catch map? m (:sel m)) (finally (bump)))))

    (is (= 4 (try+ 3 4)))
    (is (= 4 (try+ 3 4 (catch Integer i 4))))
    (is (= 4 (try+ 3 4 (finally (bump)))))
    (is (= 4 (try+ 3 4 (catch Integer i 4) (finally (bump)))))
    (is (= 5 (try+ (throw+ 4) 4 (catch Integer i (inc i)) (finally (bump)))))
    (is (= 11 @bumps))))

(defn a []
  (throw+ 1))

(defn b []
  (try+
   (a)
   (catch Integer p
     (throw+ 2))))

(defn c []
  (try+
   (b)
   (catch Integer q
     (throw+ 3))))

(defn d []
  (try+
   (c)
   (catch Integer r
     &throw-context)))

(deftest test-throw-context
  (let [context (d)]
    (is (= #{:stack :env :obj :next}
           (set (keys context))
           (set (keys (-> context :next)))
           (set (keys (-> context :next :next)))))
    (is (= 3 (-> context :obj)))
    (is (= 2 (-> context :next :obj)))
    (is (= 1 (-> context :next :next :obj)))))

(defn e []
  (try+
   (throw (Exception. "uncaught"))
   (catch Integer i i)))

(defn f []
  (try+
   (throw+ 3.2)
   (catch Integer i i)))


(defn g []
  (try+
   (throw+ 3.2 "wasn't caught")
   (catch Integer i i)))

(deftest test-uncaught
  (is (thrown-with-msg? Exception #"^uncaught$" (e)))
  (is (thrown-with-msg? slingshot.Stone #"^Object thrown by throw+.*" (f)))
  (is (thrown-with-msg? slingshot.Stone #"wasn't caught" (g))))
