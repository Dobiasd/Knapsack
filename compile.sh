#rm -r build
#mkdir build
ghc -Wall -O2 -fllvm --make -threaded -isrc -outputdir build src/Knapsack.hs -o build/Knapsack
#ghc -Wall -O2 -fllvm --make -threaded -isrc -outputdir build src/Main.hs -o build/Main