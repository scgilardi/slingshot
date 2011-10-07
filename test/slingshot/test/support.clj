(ns slingshot.test.support
  (:use [clojure.test]
        [slingshot.support :only [clause-type partition-body validate-try+-form
                                  resolved catch->cond ns-qualify
                                  make-stack-trace make-throwable]])
  (:import (java.util.concurrent ExecutionException)))

(deftest test-clause-type
  (let [f clause-type]
    (is (nil? (f 3)))
    (is (nil? (f ())))
    (is (nil? (f '(nil? x))))
    (is (= 'catch (f '(catch x))))
    (is (= 'finally (f '(finally x))))))

(deftest test-partition-body
  (let [f partition-body]
    (is (= [nil nil nil]) (f ()))
    (is (= ['(1) nil nil nil] (f '(1))))
    (is (= [nil '((catch 1)) nil nil] (f '((catch 1)))))
    (is (= [nil nil '((finally 1)) nil] (f '((finally 1)))))

    (is (= ['(1) '((catch 1)) nil nil] (f '(1 (catch 1)))))
    (is (= ['(1) nil '((finally 1)) nil] (f '(1 (finally 1)))))
    (is (= ['(1) '((catch 1)) '((finally 1)) nil]
           (f '(1 (catch 1) (finally 1)))))
    (is (= ['(1) '((catch 1) (catch 2)) '((finally 1)) nil]
           (f '(1 (catch 1) (catch 2) (finally 1)))))
    (is (= [nil '((catch 1)) nil '((1))] (f '((catch 1) (1)))))
    (is (= [nil nil '((finally 1)) '((1))] (f '((finally 1) (1)))))
    (is (= [nil nil '((finally 1)) '((catch 1))] (f '((finally 1) (catch 1)))))
    (is (= [nil nil '((finally 1) (finally 2)) nil]
           (f '((finally 1) (finally 2)))))))

(deftest test-validate-try+-form
  (let [f validate-try+-form]
    (is (nil? (f [] [] [] nil)))
    (is (nil? (f [:expr1 :expr2] [:catch1 :catch2] [:finally] nil)))
    (is (thrown? IllegalArgumentException (f [] [] [] [:more])))
    (is (thrown? IllegalArgumentException
                 (f [] [] [:finally1 :finally2] nil)))))

(deftest test-resolved
  (let [f resolved]
    (is (f 'Exception))
    (is (f 'isa?))
    (is (nil? (f 3)))
    (is (thrown? Exception (f '_)))))

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
  (make-stack-trace))

(deftest test-make-stack-trace []
  (let [{:keys [methodName className]} (-> (stack-trace-fn) first bean)]
    (is (= methodName "invoke"))
    (is (re-find #"stack_trace_fn" className))))

(deftest test-make-throwable []
  (let [tmessage "test-make-throwable-1"
        tcause (Exception.)
        tcontext {:a 1 :b 2}
        tstack-trace (make-stack-trace)
        tobject (make-throwable
                 tmessage tcause tstack-trace tcontext)
        {:keys [message cause context stackTrace]} (bean tobject)]
    (is (instance? slingshot.Stone tobject))
    (is (= [message cause context (seq stackTrace)]
           [tmessage tcause tcontext (seq tstack-trace)]))))
