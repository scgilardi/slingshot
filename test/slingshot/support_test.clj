(ns slingshot.support-test
          (:require
            [#?(:cljs cljs.pprint :clj clojure.pprint) :refer [pprint]]
            [#?(:clj  clojure.test
                :cljs cljs.test)
              :refer        [#?@(:clj [deftest is])]
              :refer-macros [deftest is]]
            [slingshot.slingshot
              :refer        [#?@(:clj [throw+ try+])]
              :refer-macros [throw+ try+]]
            [slingshot.support
              :refer        [parse-try+ stack-trace wrap
                             *throw-hook* *catch-hook*
                             #?(:clj resolve-local)]
              :refer-macros [resolve-local]])
  #?(:clj (:import (java.util.concurrent ExecutionException))))

(deftest test-parse-try+
  (let [f parse-try+]
    (is (= [nil nil nil nil] (f ())))

    (is (= ['(1) nil nil nil] (f '(1))))
    (is (= [nil '((catch 1)) nil nil] (f '((catch 1)))))
    (is (= [nil nil '(else 1) nil] (f '((else 1)))))
    (is (= [nil nil nil '(finally 1)] (f '((finally 1)))))

    (is (= ['(1) '((catch 1)) nil nil] (f '(1 (catch 1)))))
    (is (= ['(1) nil '(else 1) nil] (f '(1 (else 1)))))
    (is (= ['(1) nil nil '(finally 1)] (f '(1 (finally 1)))))

    (is (= ['(1) '((catch 1)) nil '(finally 1)]
           (f '(1 (catch 1) (finally 1)))))
    (is (= ['(1) '((catch 1) (catch 2)) nil '(finally 1)]
           (f '(1 (catch 1) (catch 2) (finally 1)))))
    (is (= ['(1) '((catch 1)) '(else 1) nil]
           (f '(1 (catch 1) (else 1)))))
    (is (= ['(1) '((catch 1) (catch 2)) '(else 1) nil]
           (f '(1 (catch 1) (catch 2) (else 1)))))

    (is (= [nil nil '(else 1) '(finally 1)]
           (f '((else 1) (finally 1)))))
    (is (= ['(1) nil '(else 1) '(finally 1)]
           (f '(1 (else 1) (finally 1)))))
    (is (= [nil '((catch 1)) '(else 1) nil]
           (f '((catch 1) (else 1)))))
    (is (= ['(1) '((catch 1)) '(else 1) nil]
           (f '(1 (catch 1) (else 1)))))

    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (f '((catch 1) (1)))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (f '((finally 1) (1)))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (f '((finally 1) (catch 1)))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (f '((finally 1) (finally 2)))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (f '((else 1) (1)))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (f '((else 1) (catch 1)))))
    (is (thrown? #?(:clj IllegalArgumentException :cljs js/Error) (f '((else 1) (else 2)))))))

#?(:clj ; FIXME CLJS doesn't always have stack traces. They're also very platform-specific
(deftest test-stack-trace
  (let [{:keys [methodName className]} (-> (stack-trace) first bean)]
    (is (= methodName "invoke"))
    (is (re-find #"stack_trace" className)))))

(deftest test-resolve-local
  (let [a 4]
    (is (= 4 (resolve-local a)))
    (is (nil? (resolve-local b)))))

(deftest test-wrap
  (let [tmessage "test-wrap-1"
        tobject 4
        tcause (#?(:clj Exception. :cljs js/Error.))
        tstack-trace (stack-trace)
        tdata {:object tobject}
        tcontext (assoc tdata
                   :message tmessage
                   :cause tcause
                   :stack-trace tstack-trace)
        tthrowable (wrap tcontext)
        {:keys [message cause data stackTrace]}
          (identity #?(:clj  (bean tthrowable)
                       :cljs {:message    (.-message tthrowable)
                              :cause      tcause
                              :data       {:object tobject}
                              :stackTrace (.-stack tthrowable)}))]
    (is (ex-data tthrowable))
    ; TODO For some reason, CLJS stack trace is different
    (is (= [message  cause  #?(:clj (seq stackTrace  )) data ]
           [tmessage tcause #?(:clj (seq tstack-trace)) tdata]))))

(def test-hooked (atom nil))

(deftest test-throw-hook
  (binding [*throw-hook* #(reset! test-hooked %)]
    (throw+ "throw-hook-string")
    (is (= (set (keys @test-hooked))
           (set [:object :message :cause :stack-trace])))
    (is (= "throw-hook-string" (:object @test-hooked))))
  (binding [*throw-hook* (fn [x] 42)]
    (is (= (throw+ "something") 42))))

(def catch-hooked (atom nil))

(defn catch-hook-return [object]
  (fn [x] (assoc x :catch-hook-return object)))

(defn catch-hook-throw [object]
  (fn [x] (assoc x :catch-hook-throw object)))

(deftest test-catch-hook
  (binding [*catch-hook* #(reset! catch-hooked %)]
    (try+ (throw+ "catch-hook-string") (catch string? x x))
    (is (= (set (keys @catch-hooked))
           (set [:object :message :cause :stack-trace :wrapper :throwable])))
    (is (= "catch-hook-string" (:object @catch-hooked))))
  (binding [*catch-hook* (catch-hook-return 42)]
    (is (= 42 (try+ (throw+ "boo") (catch string? x x)))))
  (binding [*catch-hook* (catch-hook-throw (#?(:clj IllegalArgumentException. :cljs js/Error.) "bleh"))]
    (is (thrown-with-msg? #?(:clj IllegalArgumentException :cljs js/Error) #"bleh"
                          (try+ (throw+ "boo") (catch string? x x)))))
  (is (= "soup!"
         (try+
          (binding [*catch-hook* (catch-hook-throw "soup!")]
            (try+
             (throw+ "boo")
             (catch string? x x)))
          (catch string? x x)))))
