export OPAMROOT=`pwd`/_opamroot
export OPAMYES=1
export OPAMCONFIRMLEVEL=unsafe-yes
opam init -ny --disable-sandboxing
opam switch create .
opam exec -- dune build --profile=release
