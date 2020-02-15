sbcl --no-userinit --load webserver.lisp --eval "(sb-ext:save-lisp-and-die \"webserver\" :toplevel 'main:main :executable t)"
