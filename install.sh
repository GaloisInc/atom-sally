#!/bin/bash

mkdir deps
cd deps
git clone https://github.com/GaloisInc/atom
cd ..
cabal sandbox init
cabal sandbox add-source deps/atom
cabal install --only-dependencies --enable-tests
cabal build
cabal test
