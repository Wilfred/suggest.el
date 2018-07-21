#!/bin/bash

set -e

rm -f suggest.elc

echo "Interpreted:"
cask eval "(progn (require 'suggest-bench) (suggest-bench) (suggest--possibilities-bench))"

cask eval "(byte-compile-file \"suggest.el\")"

echo -e "\nCompiled:"
cask eval "(progn (require 'suggest-bench) (suggest-bench) (suggest--possibilities-bench))"
