set -x



# Install system packages.
case $TRAVIS_OS_NAME in
    linux)
        sudo apt-get update -qq
        sudo apt-get install -qq libev-dev
        ;;
    osx)
        brew update > /dev/null
        brew install libev
        ;;
esac



# Install opam.
VERSION=2.0.5

case "$TRAVIS_OS_NAME" in
    linux) OS=linux;;
      osx) OS=macos;;
        *) echo Unsupported system $TRAVIS_OS_NAME; exit 1;;
esac

FILENAME=opam-$VERSION-x86_64-$OS

wget https://github.com/ocaml/opam/releases/download/$VERSION/$FILENAME
sudo mv $FILENAME /usr/local/bin/opam
sudo chmod a+x /usr/local/bin/opam

opam init -y --bare --disable-sandboxing --disable-shell-hook
if [ ! -d _opam/bin ]
then
    rm -rf _opam
    opam switch create . $COMPILER $REPOSITORIES --no-install
fi

eval `opam config env`
opam --version
ocaml -version



# Install dependencies.
opam install conf-libev
opam install -y --deps-only .



# Build and install Lwt_ssl. This is the only test.
make
opam lint
