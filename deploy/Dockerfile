FROM ubuntu:14.04

ENV ghc_version=7.8.4
ENV apt_ghc_version=7.8.4-8~trusty
ENV cabal_version=1.22
ENV apt_cabal_version=1.22.2.0-1~trusty

RUN apt-get update && apt-get install -q -y build-essential git npm software-properties-common python-software-properties
RUN add-apt-repository -y ppa:hvr/ghc

RUN apt-get update && apt-get install -q -y ghc-$ghc_version=$apt_ghc_version cabal-install-$cabal_version=$apt_cabal_version

ENV ghc=/opt/ghc/$ghc_version/bin/ghc
ENV cabal=/opt/cabal/$cabal_version/bin/cabal

RUN git clone https://github.com/lachenmayer/arrowsmith
WORKDIR arrowsmith

RUN npm install
RUN ./node_modules/.bin/gulp editor environment styles

RUN $cabal sandbox init
RUN $cabal install --only-dependencies