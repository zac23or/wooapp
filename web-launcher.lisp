(require :asdf)
(require :woo)

(asdf:disable-output-translations)
(defvar *port* (parse-integer (asdf::getenv "PORT")))
(load "root/quicklisp/setup.lisp")
(require :web-server)

(woo:run
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello, World")))
  :port *port*)

