(load "/root/quicklisp/setup.lisp")
(require :asdf)
(require :woo)
(require :postmodern)
(require :cl-json)
(require :cl-redis)
(require :cl+ssl)
(ql:quickload :alexandria)
(defpackage :main
  (:use cl)
  (:export :main)
  )
(in-package :main)
(defun main ()
  (load "app.lisp")
  (handler-case
    (defparameter *action* (find-symbol (string '#:response) (find-package :app)))
    (error (c) 
       (print c)
    )
  )
  (woo:run
    (lambda (env)
      (handler-case
        (funcall *action* env)
        (error (c) 
            (print c)   
           `(400 (:content-type "text/plain") (,c) )
           )
       )
    )
    :port (parse-integer (asdf::getenv "PORT"))
    :address "0.0.0.0"                      
    )
  )
