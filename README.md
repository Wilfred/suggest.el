# suggest.el

***did you mean cadr?***

[![Build Status](https://travis-ci.org/Wilfred/suggest.el.svg?branch=master)](https://travis-ci.org/Wilfred/suggest.el)
[![Coverage Status](https://coveralls.io/repos/github/Wilfred/suggest.el/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/suggest.el?branch=master)

suggest.el is an Emacs package for discovering functions based on
examples.

![suggest](suggest_screenshot.png)

## How it works

suggest.el tries your inputs (in any order) against every function in
`suggest-functions`.

`suggest-functions` is a carefully chosen function list: they're all
pure functions with a small number of arguments using only simple data
types. We only include functions that users could 'stumble upon' with
the right set of inputs.

## License

GPLv3.
