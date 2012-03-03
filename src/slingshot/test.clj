(ns slingshot.test
  (:use [clojure.test]
        [slingshot.slingshot :only [try+]]))

(defmethod assert-expr 'thrown+? [msg form]
  ;; (is (thrown+? selector expr))
  ;; Asserts that evaluating expr throws an object that matches
  ;; selector. Returns the thrown object.
  (let [selector (nth form 1)
        body (nthnext form 2)]
    `(try+
      ~@body
      (do-report {:type :fail :message ~msg :expected '~form
                  :actual "thrown+?: nothing was thrown"})
      (catch ~selector e#
        (do-report {:type :pass :message ~msg :expected '~form :actual e#})
        e#)
      (catch Object e#
        (do-report {:type :fail :message ~msg :expected '~form
                    :actual (format "thrown+?: %s did not match %s"
                                    (pr-str e#) '~selector)})
        e#))))

(defmethod assert-expr 'thrown+-with-msg? [msg form]
  ;; (is (thrown+-with-msg? s re expr))
  ;; Asserts that evaluating expr throws an object that matches
  ;; selector. Also asserts that the associated message string matches
  ;; (with re-find) the regular expression re. Returns the thrown object.
  (let [selector (nth form 1)
        re (nth form 2)
        body (nthnext form 3)]
    `(try+
      ~@body
      (do-report {:type :fail :message ~msg :expected '~form
                  :actual "thrown+-with-msg?: nothing was thrown"})
      (catch ~selector e#
        (if (re-find ~re (:message ~'&throw-context))
          (do-report {:type :pass :message ~msg :expected '~form :actual e#})
          (do-report {:type :fail :message ~msg :expected '~form
                      :actual (format "thrown+-with-msg?: %s did not match %s"
                                      (pr-str (:message ~'&throw-context))
                                      (pr-str '~re))}))
        e#)
      (catch Object e#
        (do-report {:type :fail :message ~msg :expected '~form
                    :actual (format "thrown+-with-msg?: %s did not match %s"
                                    (pr-str e#) '~selector)})
        e#))))
