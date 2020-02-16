apt update
apt install libc-dev
apt install sbcl
apt install libssl-dev
sbcl --load quicklisp.lisp --non-interactive  --eval "(quicklisp-quickstart:install)"
sbcl --load dependencies.lisp --non-interactive
