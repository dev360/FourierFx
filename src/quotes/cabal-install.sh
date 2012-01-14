#!/bin/bash 
for line in `cat cabal.requirements.txt`
  do
    cabal install $line;
done
