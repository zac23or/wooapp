apt update
apt install libc-dev
apt install sbcl
apt install libssl-dev
apt install git
apt install bcrypt
sbcl --load quicklisp.lisp --non-interactive  --eval "(quicklisp-quickstart:install)"
cd ~/quicklisp/local-projects/
git clone https://github.com/gigamonkey/monkeylib-bcrypt.git
cd -
sbcl --load dependencies.lisp --non-interactive
