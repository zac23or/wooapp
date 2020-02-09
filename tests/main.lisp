(defpackage wooapp/tests/main
  (:use :cl
        :wooapp
        :rove))
(in-package :wooapp/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :wooapp)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
