(ns slingshot.test.core
  (:use [clojure.test]
        [slingshot.core :only [try+ throw+]])
  (:import java.util.concurrent.ExecutionException))

(defrecord exception-record [error-code duration-ms message])
(defrecord x-failure [message])

(def a-sphere ^{:type ::sphere} {:radius 3})

(def h1 (derive (make-hierarchy) ::square ::shape))
(def a-square ^{:type ::square} {:size 4})

(def exception-1 (Exception. "exceptional"))
(def exception-record-1 (exception-record. 6 1000 "pdf failure"))

(defn mult-func [x y]
  (let [a 7 b 11]
    (if (= x 3)
      (* a b x y)
      (throw+ (x-failure. "x isn't 3... really??")))))

(defn test-func [x y]
  (try+
   (mult-func x y)
   (catch x-failure {message :message}
     [message (select-keys (:environment &throw-context) '(a b x y))])))

(defmacro mega-try [body]
  `(try+
    ~body

    ;; by class derived from Throwable
    (catch IllegalArgumentException e#
      [:class-iae e#])
    (catch Exception e#
      [:class-exception e#])

    ;; by java class generically
    (catch String e#
      [:class-string e#])

    ;; by clojure record type
    (catch exception-record e#
      [:class-exception-record e#])

    ;; by key-value
    (catch [:a-key 4] e#
      [:key-yields-value e#])

    ;; by key present
    (catch (contains? % :a-key) e#
      [:key-is-present e#])

    ;; by clojure type, with optional hierarchy
    (catch (isa? (type %) ::sphere) e#
      [:type-sphere (type e#) e#])
    (catch (isa? h1 (type %) ::shape) e#
      [:type-shape-in-h1 (type e#) e#])

    ;; by predicate
    (catch nil? e#
      [:pred-nil e#])
    (catch keyword? e#
      [:pred-keyword e#])
    (catch symbol? e#
      [:pred-symbol e#])
    (catch map? e#
      [:pred-map e# (meta e#)])))

(deftest test-try+

  (testing "catch by class derived from Throwable"
    (testing "treat throwables exactly as throw does, interop with try/throw"
      (is (= [:class-exception exception-1]
             (mega-try (throw+ exception-1))
             (mega-try (throw exception-1))
             (try (throw+ exception-1)
                  (catch Exception e [:class-exception e]))
             (try (throw exception-1)
                  (catch Exception e [:class-exception e])))))
    (testing "IllegalArgumentException thrown by clojure/core"
      (is (= :class-iae (first (mega-try (first 1)))))))

  (testing "catch by java class generically"
    (is (= [:class-string "fail"] (mega-try (throw+ "fail")))))

  (testing "catch by clojure record type"
    (is (= [:class-exception-record exception-record-1]
           (mega-try (throw+ exception-record-1)))))

  (testing "catch by key, with optional value"
    (is (= [:key-is-present #{:a-key}] (mega-try (throw+ #{:a-key}))))
    (is (= [:key-yields-value {:a-key 4}] (mega-try (throw+ {:a-key 4})))))

  (testing "catch by clojure type with optional hierarchy"
    (is (= [:type-sphere ::sphere a-sphere] (mega-try (throw+ a-sphere))))
    (is (= [:type-shape-in-h1 ::square a-square] (mega-try (throw+ a-square)))))

  (testing "catch by predicate"
    (is (= [:pred-nil nil] (mega-try (throw+ nil))))
    (is (= [:pred-keyword :awesome] (mega-try (throw+ :awesome))))
    (is (= [:pred-symbol 'yuletide] (mega-try (throw+ 'yuletide))))
    (is (= [:pred-map {:error-code 4} nil] (mega-try (throw+ {:error-code 4}))))
    (testing "preservation of metadata"
      (is (= [:pred-map {:error-code 4} {:severity 4}]
             (mega-try (throw+ ^{:severity 4} {:error-code 4})))))))

(deftest test-locals-and-destructuring
  (is (= 1155 (test-func 3 5)))
  (is (= ["x isn't 3... really??"
          {'x 4 'y 7 'a 7 'b 11}] (test-func 4 7))))

(deftest test-clauses
  (let [bumps (atom 0)
        bump (fn [] (swap! bumps inc))]
    (is (nil? (try+)))
    (is (nil? (try+ (catch integer? i (inc i)))))
    (is (nil? (try+ (finally (bump)))))
    (is (nil? (try+ (catch integer? i (inc i)) (finally (bump)))))
    (is (nil? (try+ (catch integer? i (inc i)) (catch map? m m)
                    (finally (bump)))))

    (is (= 3 (try+ 3)))
    (is (= 3 (try+ 3 (catch integer? i 4))))
    (is (= 3 (try+ 3 (finally (bump)))))
    (is (= 3 (try+ 3 (catch integer? i 4) (finally (bump)))))
    (is (= 4 (try+ (throw+ 3) (catch integer? i (inc i)) (finally (bump)))))
    (is (= 4 (try+ (throw+ 3) (catch integer? i (inc i)) (catch map? m m)
                   (finally (bump)))))
    (is (= 4 (try+ (throw+ {:sel 4}) (catch integer? i (inc i))
                   (catch map? m (:sel m)) (finally (bump)))))

    (is (= 4 (try+ 3 4)))
    (is (= 4 (try+ 3 4 (catch integer? i 4))))
    (is (= 4 (try+ 3 4 (finally (bump)))))
    (is (= 4 (try+ 3 4 (catch integer? i 4) (finally (bump)))))
    (is (= 5 (try+ (throw+ 4) 4 (catch integer? i (inc i)) (finally (bump)))))
    (is (= 11 @bumps))))

(defn ax [] (throw+ 1))
(defn bx [] (try+ (ax) (catch integer? p (throw+ 2))))
(defn cx [] (try+ (bx) (catch integer? q (throw+ 3))))
(defn dx [] (try+ (cx) (catch integer? r (throw+ 4))))
(defn ex [] (try+ (dx) (catch integer? s (throw+ 5))))
(defn fx [] (try+ (ex) (catch integer? t (throw+ 6))))
(defn gx [] (try+ (fx) (catch integer? u (throw+ 7))))
(defn hx [] (try+ (gx) (catch integer? v (throw+ 8))))
(defn ix [] (try+ (hx) (catch integer? w &throw-context)))

(defn next-context [x]
  (-> x :cause .getContext))

(deftest test-throw-context
  (let [context (ix)
        context1 (next-context context)
        context2 (next-context context1)]

    (is (= #{:object :message :cause :stack-trace :environment}
           (disj (set (keys context)) :wrapper)
           (set (keys context1))
           (set (keys context2))))
    (is (= 8 (-> context :object)))
    (is (= 7 (-> context1 :object)))
    (is (= 6 (-> context2 :object)))))

(defn e []
  (try+
   (throw (Exception. "uncaught"))
   (catch integer? i i)))

(defn f []
  (try+
   (throw+ 3.2)
   (catch integer? i i)))


(defn g []
  (try+
   (throw+ 3.2 "wasn't caught")
   (catch integer? i i)))

(deftest test-uncaught
  (is (thrown-with-msg? Exception #"^uncaught$" (e)))
  (is (thrown-with-msg? slingshot.Stone #"^Object thrown by throw+.*" (f)))
  (is (thrown-with-msg? slingshot.Stone #"wasn't caught" (g))))

(defn h []
  (try+
   (try+
    (throw+ 0)
    (catch zero? e
      (throw+)))
   (catch zero? e
     :zero)))

(deftest test-rethrow
  (is (= :zero (h))))

(defn i []
  (try
    (try+
     (doall (map (fn [x] (throw+ (str x))) [1]))
     (catch string? x
       x))
    (catch Throwable x)))

(defn j []
  (try+
   (let [fut (future (throw+ "whoops"))]
     @fut)
   (catch string? e
     e)))

(deftest test-issue-5
  (is (= "1" (i)))
  (is (= "whoops" (j))))

(deftest test-x-ray-vision
  (let [[val wrapper] (try+
                       (try
                         (try
                           (try
                             (throw+ "x-ray!")
                             (catch Throwable x
                               (throw (RuntimeException. x))))
                           (catch Throwable x
                             (throw (ExecutionException. x))))
                         (catch Throwable x
                           (throw (RuntimeException. x))))
                       (catch string? x
                         [x (:wrapper &throw-context)]))]
    (is (= "x-ray!" val))
    (is (= "x-ray!" (:object (-> wrapper .getCause .getCause
                                 .getCause .getContext))))))
