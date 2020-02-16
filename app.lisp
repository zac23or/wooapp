(defpackage :app
  (:use cl)
  (:export :response)
  )
(in-package :app)
(defun response (env)
  '(200 (:content-type "text/plain") ("Hello, World teste altered"))
  )
