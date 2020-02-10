(require :woo)
(defpackage :main
   (:use cl)
   (:export :main)
   )
(in-package :main)

(defun main ()
(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :port (parse-integer (asdf::getenv "PORT")))
)
