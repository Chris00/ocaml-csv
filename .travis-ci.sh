
OPAM_PKGS="oasis base-bytes lwt"

export OPAMYES=1

if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade
else
    opam init
fi

if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`

opam install $OPAM_PKGS

export OCAMLRUNPARAM=b
oasis setup

OCAML_VERSION=`ocamlc -version`
OCAML_MAJOR=`echo $OCAML_VERSION | cut -d '.' -f 1`
OCAML_MINOR=`echo $OCAML_VERSION | cut -d '.' -f 2`

if test $OCAML_MAJOR -gt 4  -o \
	\( $OCAML_MAJOR -eq 4 -a $OCAML_MINOR -ge 2 \); then
    ocaml setup.ml -configure --enable-tests --enable-lwt
    ocaml setup.ml -build
    ocaml setup.ml -test
else
    ocaml setup.ml -configure --disable-tests --enable-lwt
    ocaml setup.ml -build
fi
