(defsystem "wooapp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "wooapp/tests"))))

(defsystem "wooapp/tests"
  :author ""
  :license ""
  :depends-on ("wooapp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for wooapp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
