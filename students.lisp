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
(defvar *REDIS_URI* (uri (asdf::getenv "REDIS_URL")))
(defvar *REDIS_USER* (car(asdf::split-string (uri-userinfo *REDIS_URI*) :max 2 :separator ":")))
(defvar *REDIS_PWD* (car (last (asdf::split-string (uri-userinfo *REDIS_URI*) :max 2 :separator ":"))))
(defvar *REDIS_HOST* (uri-host *REDIS_URI*))
(defvar *REDIS_PORT* (uri-port *REDIS_URI*))
(redis:connect :host *REDIS_HOST* :port *REDIS_PORT*)
(red:auth *REDIS_PWD*)
(defvar *STUDENT_SQL* "SELECT \
 id, name, grade, classroom, balance, birthday,\
 does_not_use_password_on_terminal, gender, login, negative_limit, pdv, post_paid, registration_number, restricted_stores,\
 terminal_password,\
 imported, most_consumed_products_ids,activated_at  FROM students order by id limit $1 offset 1 * $2")

(setq *make-ssl-client-stream-verify-default* nil)
(defvar *DATABASE_URL* (uri (asdf::getenv "DATABASE_URL")))
(defvar *DATABASE_HOST* (uri-host *DATABASE_URL*))
(defvar *DATABASE_NAME* (subseq (uri-path *DATABASE_URL*) 1))
(defvar *DATABASE_PORT* (uri-port *DATABASE_URL*))
(defvar *DATABASE_USER* (car(asdf::split-string (uri-userinfo *DATABASE_URL*) :max 2 :separator ":")))
(defvar *DATABASE_PASSWORD* (car (last (asdf::split-string (uri-userinfo *DATABASE_URL*) :max 2 :separator ":"))))

(defun studentsresponse(page pageSize useredis)
    (with-connection `(,*DATABASE_NAME* ,*DATABASE_USER* ,*DATABASE_PASSWORD* ,*DATABASE_HOST*  ,:port ,*DATABASE_PORT* ,:use-ssl ,:yes)
                     (let ((st nil) (cacheident nil))
                       (setq cacheident (format nil "students-~D-~D" page pageSize))
                       (if useredis
                         (setq st (red:get cacheident))
                         )
                       (unless st
                         (setq st (json:encode-json-to-string(query *STUDENT_SQL* pageSize page :alists)))
                         )
                       (print st)
                       (if useredis
                         (red:set cacheident st)
                         )
                       `(200 (:content-type "application/json") (,st))
                       )
                     )
    )
