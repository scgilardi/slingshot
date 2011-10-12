(ns slingshot.test.support
  (:use [clojure.test]
        [slingshot.core :only [throw+ try+]]
        [slingshot.support])
  (:import (java.util.concurrent ExecutionException)))

(deftest test-part-type
  (let [f part-type]
    (is (= 'expr (f 3)))
    (is (= 'expr (f ())))
    (is (= 'expr (f '(nil? x))))
    (is (= 'catch (f '(catch x))))
    (is (= 'finally (f '(finally x))))))

(deftest test-parse
  (let [f parse]
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

(deftest test-class-name?
  (let [f class-name?]
    (is (f 'Exception))
    (is (not (f 'isa?)))
    (is (not (f 3)))
    (is (not (f '_)))))

(deftest test-catch->cond
  (let [f catch->cond]
    (is (= (f (list '_ `Exception 'e 1))
           [(list `instance? `Exception '(:object &throw-context))
            (list `let '[e (:object &throw-context)] 1)]))
    (is (= (f (list '_ `nil? 'e 1))
           [(list `nil? '(:object &throw-context))
            (list `let '[e (:object &throw-context)] 1)]))
    (is (= (f (list '_ (list :yellow (ns-qualify '%)) 'e 1))
           [(list :yellow '(:object &throw-context))
            (list `let '[e (:object &throw-context)] 1)]))))

(defn stack-trace-fn []
  (stack-trace))

(deftest test-stack-trace []
  (let [{:keys [methodName className]} (-> (stack-trace-fn) first bean)]
    (is (= methodName "invoke"))
    (is (re-find #"stack_trace_fn" className))))

(deftest test-make-throwable []
  (let [tmessage "test-make-throwable-1"
        tcause (Exception.)
        tcontext {:a 1 :b 2}
        tstack-trace (stack-trace)
        tobject (make-throwable
                 tmessage tcause tstack-trace tcontext)
        {:keys [message cause context stackTrace]} (bean tobject)]
    (is (instance? slingshot.Stone tobject))
    (is (= [message cause context (seq stackTrace)]
           [tmessage tcause tcontext (seq tstack-trace)]))))


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
