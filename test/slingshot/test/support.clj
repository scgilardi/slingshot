(ns slingshot.test.support
  (:use [clojure.test]
        [slingshot.core :only [throw+ try+]]
        [slingshot.support])
  (:import (java.util.concurrent ExecutionException)))

(deftest test-try-item-type
  (let [f try-item-type]
    (is (= :expression (f 3)))
    (is (= :expression (f ())))
    (is (= :expression (f '(nil? x))))
    (is (= :catch-clause (f '(catch x))))
    (is (= :finally-clause (f '(finally x))))))

(deftest test-parse-try
  (let [f parse-try]
    (is (= [nil nil nil]) (f ()))
    (is (= ['(1) nil nil] (f '(1))))
    (is (= [nil '((catch 1)) nil] (f '((catch 1)))))
    (is (= [nil nil '((finally 1))] (f '((finally 1)))))

    (is (= ['(1) '((catch 1)) nil] (f '(1 (catch 1)))))
    (is (= ['(1) nil '((finally 1))] (f '(1 (finally 1)))))
    (is (= ['(1) '((catch 1)) '((finally 1))]
           (f '(1 (catch 1) (finally 1)))))
    (is (= ['(1) '((catch 1) (catch 2)) '((finally 1))]
           (f '(1 (catch 1) (catch 2) (finally 1)))))
    (is (thrown? IllegalArgumentException (f '((catch 1) (1)))))
    (is (thrown? IllegalArgumentException (f '((finally 1) (1)))))
    (is (thrown? IllegalArgumentException (f '((finally 1) (catch 1)))))
    (is (thrown? IllegalArgumentException (f '((finally 1) (finally 2)))))))

(deftest test-selector-type
  (let [f selector-type]
    (is (= :class-name (f 'Integer)))
    (is (= :key-value (f [:type :terrific])))
    (is (= :form) (f `(:one :two % :four)))
    (is (= :predicate (f nil?)))))

(deftest test-cond-test-expression
  (let [f cond-test-expression]
    (binding [*ns* (the-ns 'slingshot.test.support)]
      (is (= (f (list '_ `Exception 'e 1))
             [(list `instance? `Exception '(:object &throw-context))
              (list `let '[e (:object &throw-context)] 1)]))
      (is (= (f (list '_ `nil? 'e 1))
             [(list `nil? '(:object &throw-context))
              (list `let '[e (:object &throw-context)] 1)]))
      (is (= (f (list '_ (list :yellow '%) 'e 1))
             [(list :yellow '(:object &throw-context))
              (list `let '[e (:object &throw-context)] 1)])))))

(deftest test-parse-key-value
  (let [f parse-key-value]
    (is (thrown? IllegalArgumentException (f [])))
    (is (thrown? IllegalArgumentException (f [:a])))
    (is (= [:a :b] (f [:a :b])))
    (is (thrown? IllegalArgumentException (f [:a :b :c])))))

(defn stack-trace-fn []
  (stack-trace))

(deftest test-stack-trace []
  (let [{:keys [methodName className]} (-> (stack-trace-fn) first bean)]
    (is (= methodName "invoke"))
    (is (re-find #"stack_trace_fn" className))))

(deftest test-make-throwable []
  (let [tmessage "test-make-throwable-1"
        tobject 4
        tcause (Exception.)
        tstack-trace (stack-trace)
        tcontext {:message tmessage
                  :object tobject
                  :cause tcause
                  :stack-trace tstack-trace}
        tthrowable (make-throwable tcontext)
        {:keys [message cause context stackTrace]} (bean tthrowable)]
    (is (instance? slingshot.Stone tthrowable))
    (is (= [message cause (seq stackTrace) context]
           [(throwable-message tcontext) tcause (seq tstack-trace) tcontext]))))

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
