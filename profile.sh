#!/bin/bash
ghc -prof -auto-all -caf-all -fforce-recomp -O2 -fllvm --make -threaded -isrc -outputdir build src/Main.hs -o build/Main
./build/Main +RTS -p