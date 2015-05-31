FROM ubuntu:14.04

RUN apt-get update && apt-get install -q -y language-pack-en
RUN update-locale LANG=en_US.UTF-8

RUN apt-get install -q -y build-essential curl git pigz zlib1g-dev

RUN mkdir -p /app

RUN git clone https://github.com/mietek/halcyon /app/halcyon
RUN /app/halcyon/halcyon install https://github.com/lachenmayer/arrowsmith --purge-cache --ghc-version 7.8.4 --cabal-version 1.22.0.1 --sandbox-sources="elm-compiler elm-make snap-extras-0.9"