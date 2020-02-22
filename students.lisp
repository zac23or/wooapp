(defpackage :students
  (:use cl)
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
  (let ((st nil) (cacheident nil))
    (setq cacheident (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t))
    (print cacheident)
    (with-output-to-string (s cacheident)
      (format s "students-~D-~D" page pageSize)
      (input-stream-p s)
      (print cacheident)
    )
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
  
  
