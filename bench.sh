#!/bin/bash

set -ex

rm -f suggest.elc

cask eval "(progn (require 'suggest-bench) (suggest-bench))"
