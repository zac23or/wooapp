(load "./students.lisp")
(load "./rd.lisp")
(load "./jsn.lisp")
(defpackage :app
  (:use cl)
  (:use asdf)
  (:use quri)
  (:use cl-json)
  (:use cl+ssl)
  (:use postmodern)
  (:use students)
  (:use rd)
  (:use jsn)
  (:export :response)
  )
(in-package :app)
(defvar *html* "<html>\
      <body>\
        <a href='/pg'>Teste abertura/fechamento conexão pg</a> <br>\
        <h1>PG puro</h1>
        <a href='/students?pageSize=40'>Teste pega 40 estudantes no pg</a> <br>\
        <a href='/students?pageSize=20'>Teste pega 20 estudantes no pg</a> <br>\
        <a href='/students?pageSize=10'>Teste pega 10 estudantes no pg</a> <br>\
        <a href='/students?pageSize=1'>Teste pega 1 estudante no pg</a> <br>\
        <h1>PG com redis</h1>\
        <a href='/students?pageSize=40&redis=true'>Teste pega 40 estudantes no pg</a> <br>\
        <a href='/students?pageSize=20&redis=true'>Teste pega 20 estudantes no pg</a> <br>\
        <a href='/students?pageSize=10&redis=true'>Teste pega 10 estudantes no pg</a> <br>\
        <a href='/students?pageSize=1&redis=true'>Teste pega 1 estudantes no pg</a> <br>\
        <h1>Redis</h1>\
        <a href='/rd'>Teste abertura/fechamento conexão redis</a><br>\
        <a href='/rdsg'>Teste setar/pegar valor em chave redis</a><br>\
        <h1>BCrypt</h1>\
        <a href='/bcrypt'>Teste login com bcrypt</a><br>\
        <h1>JSON</h1>\
        <a href='/json'>Test 40 dict para json</a><br>\
      </body>\
      </html>")
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun querystringparam(querystring param)
  (let ((params (asdf::split-string querystring :max 1000 :separator "&")))
    (getf (mapcar #'intern (flatten (mapcar #'(lambda(item) (asdf::split-string item :max 1000 :separator "="))params))) (intern param))
    )
  )
    
(defun page(env)
   (or (querystringparam (getf env :query-string) "page") 1)
  )
(defun pageSize(env)
   (or (querystringparam (getf env :query-string) "pageSize") 20)
  )

(defun redis(env)
    (string (or (querystringparam (getf env :query-string) "redis") nil))
  )
(defun response (env)
  (if (equal (getf env :path-info) "/students")
   (studentsresponse (page env) (pageSize env) (redis env))
  `(200 (:content-type "text/html") (,*html*))
  )
)
(setq *make-ssl-client-stream-verify-default* nil)
(let ((uri (uri (asdf::getenv "DATABASE_URL"))))
  (let ((db (subseq (uri-path uri) 1)) (port (uri-port uri)) (dbuser (car(asdf::split-string (uri-userinfo uri) :max 2 :separator ":"))) 
                                       (dbpassword (car (last (asdf::split-string (uri-userinfo uri) :max 2 :separator ":")))) (host (uri-host uri)))
    (print (connect-toplevel db dbuser dbpassword host :port port :use-ssl :yes))
    )
  )
