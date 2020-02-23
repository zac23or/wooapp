(defpackage :students
  (:use cl)
  (:use quri)
  (:use cl+ssl)
  (:use postmodern)
  (:local-nicknames (:rc :redis))
  (:use cl-json)
  (:export :studentsresponse)
  )
(in-package :students)
(redis:connect :host "localhost")
(defvar *STUDENT_SQL* "SELECT \
 id, name, grade, classroom, balance, birthday,\
 does_not_use_password_on_terminal, gender, login, negative_limit, pdv, post_paid, registration_number, restricted_stores,\
 terminal_password,\
 imported, most_consumed_products_ids,activated_at  FROM students order by id limit $1 offset 1 * $2")

(defun studentsresponse(page pageSize useredis)
  (print (format nil "page:~A pageSize:~A redis: ~A" page pageSize useredis))
  (let ((st nil) (cacheident nil))
    (setq cacheident (format nil "students-~D-~D" page pageSize))
    (if useredis
      (setq st (red:get cacheident))
      )
    (unless st
      (setq st (json:encode-json-to-string(query *STUDENT_SQL* pageSize page :alists)))
      )
    (if useredis
      (red:set cacheident st)
      )
    `(200 (:content-type "application/json") (,st))
    )
  )


(setq *make-ssl-client-stream-verify-default* nil)
(let ((uri (uri (asdf::getenv "DATABASE_URL"))))
  (let ((db (subseq (uri-path uri) 1)) (port (uri-port uri))
        (dbuser (car(asdf::split-string
		     (uri-userinfo uri) :max 2 :separator ":")))
	(dbpassword (car (last (asdf::split-string (uri-userinfo uri) :max 2 :separator ":")))) (host (uri-host uri)))
    (connect-toplevel db dbuser dbpassword host :port port :use-ssl :yes)
    )
  )
