FROM ubuntu:xenial
MAINTAINER "Nathan Bedell" "nbedell@tulane.edu"
CMD apt -y update
CMD apt -y install ghc \
                   cabal-install \
                   git \
                   software-properties-common
CMD add-apt-repository -y ppa:hvr/ghc
CMD apt update
CMD apt -y install ghc-8.4.1
CMD cabal update
WORKDIR /root/data/
CMD git clone https://github.com/Sintrastes/xen-toolbox
WORKDIR /root/data/xen-toolbox/
CMD cabal install --with-ghc=/opt/ghc/bin/ghc-8.4.1