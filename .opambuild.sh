export OPAMROOT=`pwd`/_opamroot
export OPAMYES=1
export OPAMCONFIRMLEVEL=unsafe-yes
opam init -ny --disable-sandboxing --bare
opam switch create . 5.3.0 --deps-only
opam exec -- dune build --profile=release
