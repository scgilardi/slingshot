(ns slingshot.test.support
  (:use [clojure.test]
        [slingshot.slingshot :only [throw+ try+]]
        [slingshot.support])
  (:import (java.util.concurrent ExecutionException)))

(deftest test-parse-try+
  (let [f parse-try+]
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

(defn stack-trace-fn []
  (stack-trace))

(deftest test-stack-trace []
  (let [{:keys [methodName className]} (-> (stack-trace-fn) first bean)]
    (is (= methodName "invoke"))
    (is (re-find #"stack_trace_fn" className))))

(deftest test-wrap []
  (let [tmessage "test-wrap-1"
        tobject 4
        tcause (Exception.)
        tstack-trace (stack-trace)
        tdata {:message tmessage
                  :object tobject
                  :cause tcause
                  :stack-trace tstack-trace}
        tthrowable (wrap tdata)
        {:keys [message cause data stackTrace]} (bean tthrowable)]
    (is (instance? slingshot.ExceptionInfo tthrowable))
    (is (= [message cause (seq stackTrace) data]
           [tmessage tcause (seq tstack-trace) tdata]))))

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
  (fn [x] (assoc x :catch-hook-return object)))

(defn catch-hook-throw [object]
  (fn [x] (assoc x :catch-hook-throw object)))

(deftest test-catch-hook
  (binding [*catch-hook* #(reset! catch-hooked %)]
    (try+ (throw+ "catch-hook-string") (catch string? x x))
    (is (= (set (keys @catch-hooked))
           (set [:throwable :object :message :cause :stack-trace :environment])))
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
