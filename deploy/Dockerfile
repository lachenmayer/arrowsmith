FROM ubuntu:14.04

ENV ghc_version=7.8.4
ENV apt_ghc_version=7.8.4-8~trusty
ENV cabal_version=1.22
ENV apt_cabal_version=1.22.2.0-1~trusty

# basics & add-apt-repository command
RUN apt-get update && apt-get install -q -y build-essential pigz zlib1g-dev git software-properties-common python-software-properties

# ghc & node
RUN add-apt-repository -y ppa:hvr/ghc
RUN add-apt-repository -y ppa:chris-lea/node.js
RUN apt-get update && apt-get install -q -y ghc-$ghc_version=$apt_ghc_version cabal-install-$cabal_version=$apt_cabal_version nodejs

ENV PATH=/opt/ghc/$ghc_version/bin:/opt/cabal/$cabal_version/bin:$PATH

# arrowsmith
RUN git clone https://github.com/lachenmayer/arrowsmith
WORKDIR arrowsmith

RUN cabal sandbox init
RUN cabal sandbox add-source elm-make
RUN cabal sandbox add-source elm-compiler
RUN cabal sandbox add-source snap-extras-0.9
RUN cabal update && cabal install --only-dependencies

RUN npm install
RUN ./node_modules/.bin/gulp editor environment styles

RUN cabal build