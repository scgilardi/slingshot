(ns slingshot.slingshot-test
           (:require
             [#?(:clj  clojure.test
                 :cljs cljs.test)
               :refer        [#?@(:clj [deftest testing is])]
               :refer-macros [deftest testing is async]]
             [clojure.string :as str]
             [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint]]
             [cljs.compiler]
    #?(:cljs [cljs.core.async :refer [<!]])
             [slingshot.slingshot
               :refer        [#?@(:clj [throw+ try+])
                              get-throw-context
                              get-thrown-object]
               :refer-macros [throw+ try+]]
             [slingshot.util
               :refer [#?@(:clj [if-cljs when-cljs])]])
  #?(:cljs (:require-macros
             [slingshot.slingshot-test
               :refer [mega-try caught-result caught-result-from-catch]]
             [cljs.core.async.macros
               :refer [go]]))
  #?(:clj  (:import java.util.concurrent.ExecutionException)))

(defrecord exception-record [error-code duration-ms message])
(defrecord x-failure [message])

(def a-sphere ^{:type ::sphere} {:radius 3})

(def h1 (derive (make-hierarchy) ::square ::shape))
(def a-square ^{:type ::square} {:size 4})

(def exception-1 (#?(:clj Exception. :cljs js/TypeError.) "exceptional"))
(def exception-record-1 (exception-record. 6 1000 "pdf failure"))

(defn mult-func [x y]
  (let [a 7 b 11]
    (if (= x 3)
      (* a b x y)
      (throw+ (x-failure. "x isn't 3... really??")))))

(defn meta-type [x] (-> x meta :type))

(defn get-message [e]
  #?(:clj  (.getMessage ^Throwable e)
     :cljs (.-message e)))

#?(:cljs
(defn ->Error
  ([x] (cond (instance? js/Error x)
             (->Error (.-message x) x)
             (string? x)
             (js/Error. x)
             :else (throw (ex-info "Invalid constructor for js/Error" {:args [x]}))))
  ([message cause]
    (doto (js/Error. message)
          (-> .-cause (set! cause))))))

#?(:clj
(defmacro mega-try [body]
  (let [type-fn (if-cljs &env `meta-type `type)]
    `(try+
      ~body

      ;; by class derived from Throwable / js/Error
      ~@(if-cljs &env
          `[]
          `[(catch IllegalArgumentException e#
              [:class-iae e#])])
      (catch ~(if-cljs &env `js/TypeError `Exception) e#
        [:class-exception e#])
      
      ;; by java class generically
      (catch ~(if-cljs &env `string? `String) e#
        [:class-string e#])
      
      ;; by clojure record type
      (catch exception-record e#
        [:class-exception-record e#])

      ;; by key-value
      (catch [:a-key 4] e#
        [:key-yields-value e#])

      ;; by multiple-key-value
      (catch [:key1 4 :key2 5] e#
        [:keys-yield-values e#])

      ;; by key present
      (catch (and (set? ~'%) (contains? ~'% :a-key)) e#
        [:key-is-present e#])

      ;; by clojure type, with optional hierarchy
      (catch (isa? (~type-fn ~'%) ::sphere) e#
        [:type-sphere (~type-fn e#) e#])
      (catch (isa? h1 (~type-fn ~'%) ::shape) e#
        [:type-shape-in-h1 (~type-fn e#) e#])

      ;; by predicate
      (catch nil? e#
        [:pred-nil e#])
      (catch keyword? e#
        [:pred-keyword e#])
      (catch symbol? e#
        [:pred-symbol e#])
      (catch map? e#
        [:pred-map e# (meta e#)])))))

(deftest test-try+
  (testing "catch by class derived from Throwable"
    (testing "treat throwables exactly as throw does, interop with try/throw"
      (is (= [:class-exception exception-1]
             (mega-try (throw+ exception-1))
             (mega-try (throw exception-1))
             (try (throw+ exception-1)
                  (catch #?(:clj Exception :cljs js/TypeError) e [:class-exception e]))
             (try (throw exception-1)
                  (catch #?(:clj Exception :cljs js/TypeError) e [:class-exception e])))))
    #?(:clj (testing "IllegalArgumentException thrown by clojure/core"
              (is (= :class-iae (first (mega-try (str/replace "foo" 1 1))))))))

  (testing "catch by java class generically"
    (is (= [:class-string "fail"] (mega-try (throw+ "fail")))))

  (testing "catch by clojure record type"
    (is (= [:class-exception-record exception-record-1]
           (mega-try (throw+ exception-record-1)))))

  (testing "catch by key is present"
    (is (= [:key-is-present #{:a-key}] (mega-try (throw+ #{:a-key})))))

  (testing "catch by keys and values"
    (is (= [:key-yields-value {:a-key 4}] (mega-try (throw+ {:a-key 4}))))
    (is (= [:keys-yield-values {:key1 4 :key2 5}]
           (mega-try (throw+ {:key1 4 :key2 5})))))

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
  (-> x :cause get-throw-context))


(deftest test-throw-context
  (let [context (ix)
        context1 (next-context context)
        context2 (next-context context1)]

    (is (= #{:object :message :cause :stack-trace :wrapper :throwable}
           (set (keys context))
           (set (keys context1))
           (set (keys context2))))
    (is (= 8 (-> context :object)))
    (is (= 7 (-> context1 :object)))
    (is (= 6 (-> context2 :object)))))

(defn e []
  (try+
   (throw (#?(:clj Exception. :cljs js/Error.) "uncaught"))
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
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"^uncaught$" (e)))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"^throw\+: .*" (f)))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"wasn't caught" (g))))

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
    (catch #?(:clj Throwable :cljs js/Error) x)))

#?(:clj
(defn j
  "NOTE: Doesn't work in CLJS. Uncaught exceptions in `go` blocks
   (the semi-equivalent of Clojure's `future`) aren't returned when
   taken e.g. via `<!` — only nil is returned. See also
   http://dev.clojure.org/jira/browse/ASYNC-172."
  []
  (try+
    (let [fut (future (throw+ "whoops"))]
      @fut)
     (catch string? e e))))

#?(:clj
(deftest test-issue-5
  (is (= "1" (i)))
  (is (= "whoops" (j)))))

(deftest test-unmacroed-pct
  (is (= :was-eee (try+ (throw+ "eee")
                        (catch (= % "eee") _ :was-eee)
                        (catch string? _ :no!)))))

(deftest test-x-ray-vision []
  (let [[val wrapper] (try+
                       (try
                         (try
                           (try
                             (throw+ "x-ray!")
                             (catch #?(:clj Throwable :cljs :default) x
                               (throw (#?(:clj RuntimeException. :cljs ->Error) x))))
                           (catch #?(:clj Throwable :cljs :default) x
                             (throw (#?(:clj ExecutionException. :cljs ->Error) x))))
                         (catch #?(:clj Throwable :cljs :default) x
                           (throw (#?(:clj RuntimeException. :cljs ->Error) x))))
                       (catch string? x
                         [x (:throwable &throw-context)]))]
    (is (= "x-ray!" val))
    (is (= "x-ray!" (get-thrown-object wrapper)))))

(deftest test-catching-wrapper
  (let [e (#?(:clj Exception. :cljs js/Error.))]
    (try
      (try+
       (throw e)
       (catch #?(:clj Exception :cljs js/Error) _
         (throw+ :a "msg: %s" %)))
      (is false)
      (catch #?(:clj Exception :cljs js/Error) s
        (is (= "msg: :a" (get-message s)))
        (is (= e (#?(:clj .getCause :cljs .-cause) s)))))))

(deftest test-eval-object-once
  (let [bumps (atom 0)
        bump (fn [] (swap! bumps inc))]
    (try+
     (throw+ (bump) "this is it: %s %s %s" % % %)
     (catch #?(:clj Object :cljs (constantly true)) _))
    (is (= @bumps 1))))

(deftest test-get-throw-context
  (let [object (#?(:clj Object. :cljs js/Object.))
        exception1 (#?(:clj Exception. :cljs js/Error.))
        exception2 (#?(:clj Exception. :cljs ->Error) "ex1" exception1)
        t1 (try
             (throw+ object)
             (catch #?(:clj Throwable :cljs js/Error) t t))
        t2 (try
             (throw+ exception2)
             (catch #?(:clj Throwable :cljs js/Error) t t))
        t3 (try
             (throw exception2)
             (catch #?(:clj Throwable :cljs js/Error) t t))]
    (is (= #{:object :message :cause :stack-trace :wrapper
             :throwable}
           (-> t1 get-throw-context keys set)))
    (is (= #{:object :message :cause :stack-trace :throwable}
           (-> t2 get-throw-context keys set)))
    (is (= #{:object :message :cause :stack-trace :throwable}
           (-> t3 get-throw-context keys set)))

    (is (identical? object (:object (get-throw-context t1))))
    (is (identical? exception2 (:object (get-throw-context t2))))
    (is (identical? exception2 (:object (get-throw-context t3))))

    (is (identical? exception1 (:cause (get-throw-context t2))))
    (is (identical? exception1 (:cause (get-throw-context t3))))
    (is (= "ex1" (:message (get-throw-context t2))))
    (is (= "ex1" (:message (get-throw-context t3))))))

(deftest test-get-thrown-object
  (let [object (#?(:clj Object. :cljs js/Object.))
        exception (#?(:clj Exception. :cljs js/Error.))
        t1 (try
             (throw+ object)
             (catch #?(:clj Throwable :cljs js/Error) t t))
        t2 (try
             (throw+ exception)
             (catch #?(:clj Throwable :cljs js/Error) t t))
        t3 (try
             (throw exception)
             (catch #?(:clj Throwable :cljs js/Error) t t))]
    (is (identical? object (get-thrown-object t1)))
    (is (identical? exception (get-thrown-object t2)))
    (is (identical? exception (get-thrown-object t3)))))

(deftest test-wrapper-and-throwable
  (let [context (try+
                 (try
                   (throw+ :afp "wrapper-0")
                   (catch #?(:clj Exception :cljs js/Error) e
                     (throw (#?(:clj RuntimeException. :cljs ->Error) "wrapper-1" e))))
                 (catch #?(:clj Object :cljs (constantly true)) _
                   &throw-context))]
    (is (= "wrapper-0" (get-message (:wrapper context))))
    (is (= "wrapper-1" (get-message (:throwable context))))))

(deftest test-inline-predicate
  (is (= :not-caught (try+
                      (throw+ {:foo true})
                      (catch #(-> % :foo (= false)) data
                        :caught)
                      (catch #?(:clj Object :cljs (constantly true)) _
                        :not-caught)))))

(defn gen-body
  [rec-sym throw?]
  (let [body `(swap! ~rec-sym #(conj % :body))]
    (if throw?
      (list 'do body `(throw+ (#?(:clj Exception. :cljs js/Error.))))
      body)))

(defn gen-catch-clause
  [rec-sym]
  `(catch #?(:clj Exception :cljs js/Error) e# (swap! ~rec-sym #(conj % :catch))))

(defn gen-else-clause
  [rec-sym broken?]
  (let [else-body `(swap! ~rec-sym #(conj % :else))]
    (if broken?
      (list 'else (list 'do else-body `(throw+ (#?(:clj Exception. :cljs js/Error.)))))
      (list 'else else-body))))

(defn gen-finally-clause
  [rec-sym]
  `(finally (swap! ~rec-sym #(conj % :finally))))

(defn gen-try-else-form
  "Generate variations of (try ... (else ...) ...) forms, which (when eval'd)
  will return a vector describing the sequence in which things were evaluated,
  e.g. [:body :catch :finally]"
  [throw? catch? finally? broken-else?]
  (let [rec-sym (gensym "rec")
        body (gen-body rec-sym throw?)
        catch-clause (if catch? (gen-catch-clause rec-sym))
        else-clause (gen-else-clause rec-sym broken-else?)
        finally-clause (if finally? (gen-finally-clause rec-sym))]
    `(let [~rec-sym (atom [])]
       (try+
        ~(remove nil? `(try+
                        ~body
                        ~catch-clause
                        ~else-clause
                        ~finally-clause))
        (catch #?(:clj Object :cljs (constantly true)) e#
          ;; if the inner try+ threw, report it as a :bang! in the return vec
          (swap! ~rec-sym #(conj % :bang!))))
       @~rec-sym)))

#?(:clj ; FIXME Uses `eval`, which is only in Clojure
(deftest test-else
  (doseq [throw? [true false]
          catch? [true false]
          broken-else? [true false]
          finally? [true false]]
    (testing (str "test-else: throw? " throw? " catch? " catch?
                  " broken-else? " broken-else? " finally? " finally?)
      (let [try-else-form (gen-try-else-form throw? catch? finally? broken-else?)
            actual (eval try-else-form)
            expected (vec (remove nil?
                                  [:body
                                   (if (and throw? catch?) :catch)
                                   (if (not throw?) :else)
                                   (if finally? :finally)
                                   ;; expect an escaped exception when either:
                                   ;;  a) the else clause runs, and throws
                                   ;;  b) the body throws, and is not caught
                                   (if (or (and (not throw?) broken-else?)
                                           (and throw? (not catch?))) :bang!)]))]
        (is (= actual expected)))))))

#?(:clj ; FIXME Reflection is only in Clojure
(deftest test-reflection
  (try+
   nil
   (catch Exception e
     (.getMessage e)))))

(deftest test-ex-info-compatibility
  (let [data {:type :fail :reason :not-found}
        message "oops"
        wrapper (ex-info message data)
        rte1 (#?(:clj RuntimeException. :cljs ->Error) "one" wrapper)
        rte2 (#?(:clj RuntimeException. :cljs ->Error) "two" rte1)
        direct (try+
                (throw wrapper)
                (catch [:type :fail] e
                  &throw-context)
                (catch #?(:clj Object :cljs (constantly true)) _
                  :whoops))
        cause-chain (try+
                     (throw rte2)
                     (catch [:type :fail] e
                       &throw-context)
                     (catch #?(:clj Object :cljs (constantly true)) _
                       :whoops))]
    (is (= (:object direct) data))
    (is (= (:object cause-chain) data))
    (is (= (:message direct) message))
    (is (= (:message cause-chain) message))
    (is (= (:wrapper direct) wrapper))
    (is (= (:wrapper cause-chain) wrapper))
    (is (= (:throwable direct) wrapper))
    (is (= (:throwable cause-chain) rte2))))

;; helpers for test-optional-cause

#?(:clj
(defmacro caught-result [& body]
  `(try+
    ~@body
    (catch ~(if-cljs &env `(constantly true) `Object) ~'o
      [(:cause ~'&throw-context)
       (:message ~'&throw-context)]))))

#?(:clj
(defmacro caught-result-from-catch [cause & body]
  `(caught-result
    (try+
     (throw+ ~cause)
     (catch ~(if-cljs &env `(constantly true) `Object) ~'o
       ~@body)))))

(deftest test-optional-cause
  (let [imp (#?(:clj Exception. :cljs js/Error.) "I did it implicitly.")
        exp (#?(:clj Exception. :cljs js/Error.) "I did it explicitly.")
        def-msg "throw+: 1"
        msg "message two %s"
        fmt "aha! %s"
        fmt-msg "aha! 1"
        fmt2 "%s leading to %s"
        fmt2-msg "1 leading to [1 1]"

        ;; throw from outside catch, no implicit cause

        result1 (caught-result (throw+ 1))
        result2 (caught-result (throw+ 1 msg))
        result3 (caught-result (throw+ 1 fmt %))
        result4 (caught-result (throw+ 1 fmt2 % [% %]))

        result5 (caught-result (throw+ 1 nil))
        result6 (caught-result (throw+ 1 nil msg))
        result7 (caught-result (throw+ 1 nil fmt %))
        result8 (caught-result (throw+ 1 nil fmt2 % [% %]))

        result9 (caught-result (throw+ 1 exp))
        result10 (caught-result (throw+ 1 exp msg))
        result11 (caught-result (throw+ 1 exp fmt %))
        result12 (caught-result (throw+ 1 exp fmt2 % [% %]))

        ;; throw from inside catch, implicit cause available

        result13 (caught-result-from-catch imp (throw+))

        result14 (caught-result-from-catch imp (throw+ 1))
        result15 (caught-result-from-catch imp (throw+ 1 msg))
        result16 (caught-result-from-catch imp (throw+ 1 fmt %))
        result17 (caught-result-from-catch imp (throw+ 1 fmt2 % [% %]))

        result18 (caught-result-from-catch imp (throw+ 1 nil))
        result19 (caught-result-from-catch imp (throw+ 1 nil msg))
        result20 (caught-result-from-catch imp (throw+ 1 nil fmt %))
        result21 (caught-result-from-catch imp (throw+ 1 nil fmt2 % [% %]))

        result22 (caught-result-from-catch imp (throw+ 1 exp))
        result23 (caught-result-from-catch imp (throw+ 1 exp msg))
        result24 (caught-result-from-catch imp (throw+ 1 exp fmt %))
        result25 (caught-result-from-catch imp (throw+ 1 exp fmt2 % [% %]))]

    (testing "outside catch"
      (testing "implicit cause"
        (is (= result1 [nil def-msg]))
        (is (= result2 [nil msg]))
        (is (= result3 [nil fmt-msg]))
        (is (= result4 [nil fmt2-msg])))
      (testing "erased cause"
        (is (= result5 [nil def-msg]))
        (is (= result6 [nil msg]))
        (is (= result7 [nil fmt-msg]))
        (is (= result8 [nil fmt2-msg])))
      (testing "explicit cause"
        (is (= result9 [exp def-msg]))
        (is (= result10 [exp msg]))
        (is (= result11 [exp fmt-msg]))
        (is (= result12 [exp fmt2-msg]))))
    (testing "inside catch"
      (testing "rethrow"
        (is (= result13 [nil "I did it implicitly."])))
      (testing "implicit cause"
        (is (= result14 [imp def-msg]))
        (is (= result15 [imp msg]))
        (is (= result16 [imp fmt-msg]))
        (is (= result17 [imp fmt2-msg])))
      (testing "erased cause"
        (is (= result18 [nil def-msg]))
        (is (= result19 [nil msg]))
        (is (= result20 [nil fmt-msg]))
        (is (= result21 [nil fmt2-msg])))
      (testing "explicit cause"
        (is (= result22 [exp def-msg]))
        (is (= result23 [exp msg]))
        (is (= result24 [exp fmt-msg]))
        (is (= result25 [exp fmt2-msg]))))))
