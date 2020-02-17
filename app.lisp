(defpackage :app
  (:use cl)
  (:use asdf)
  (:use quri)
  (:use cl-json)
  (:use cl+ssl)
  (:use postmodern)
  (:export :response)
  )
(in-package :app)
(defvar *STUDENT_SQL* "SELECT \
 id, name, grade, classroom, balance, birthday,\
 does_not_use_password_on_terminal, gender, login, negative_limit, pdv, post_paid, registration_number, restricted_stores,\
 terminal_password,\
 imported, most_consumed_products_ids,activated_at  FROM students order by id limit $1 offset 1 * $2") 
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun querystringparam(querystring param)
  (let ((params (asdf::split-string querystring :max 1000 :separator "&")))
    (getf (mapcar #'intern (flatten (mapcar #'(lambda(item) (asdf::split-string item :max 1000 :separator "="))params))) (intern param))
    )
  )
    
(defun page (env)
   (string (or (querystringparam (getf env :query-string) "page") 1))
  )
(defun pageSize(env)
    (string (or (querystringparam (getf env :query-string) "pageSize") 20))
  )
(defun response (env)
   (print env)
  `(200 (:content-type "text/plain") (,(json:encode-json-to-string(query *STUDENT_SQL* (pageSize env) (page env)))))
  )

(setq *make-ssl-client-stream-verify-default* nil)
(let ((uri (uri (asdf::getenv "DATABASE_URL"))))
  (let ((db (subseq (uri-path uri) 1)) (port (uri-port uri)) (dbuser (car(asdf::split-string (uri-userinfo uri) :max 2 :separator ":"))) 
                                       (dbpassword (car (last (asdf::split-string (uri-userinfo uri) :max 2 :separator ":")))) (host (uri-host uri)))
    (print (connect-toplevel db dbuser dbpassword host :port port :use-ssl :yes))
    )
  )
