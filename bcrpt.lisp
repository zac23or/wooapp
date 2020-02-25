
(defpackage :bcrpt
  (:use cl)
  (:local-nicknames (:rds :redis))
  (:use bcrypt)
  (:export :bcrptresponse)
  )
(in-package :bcrpt)
(defun bcrptresponse()

  (let ((now (get-internal-real-time)) (text ""))
    (bcrypt:hash "plain text password")
    (setq text (write-to-string (- (get-internal-real-time) now)))
    `(200 (:content-type "text/html") (,text))

    )
  )


