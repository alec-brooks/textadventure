#!/bin/bash
find . -name "*hs" | entr cabal test
