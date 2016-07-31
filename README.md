# suggest.el

***did you mean cadr?***

[![Build Status](https://travis-ci.org/Wilfred/suggest.el.svg?branch=master)](https://travis-ci.org/Wilfred/suggest.el)
[![Coverage Status](https://coveralls.io/repos/github/Wilfred/suggest.el/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/suggest.el?branch=master)
[![MELPA](http://melpa.org/packages/suggest-badge.svg)](http://melpa.org/#/suggest)

suggest.el is an Emacs package for discovering elisp functions based
on examples. See my blog post:
[Example Driven Development](http://www.wilfred.me.uk/blog/2016/07/30/example-driven-development/).

![suggest](suggest_screenshot.png)

## How it works

suggest.el tries your inputs (in any order) against every function in
`suggest-functions`.

`suggest-functions` is a carefully chosen function list: they're all
pure functions with a small number of arguments using only simple data
types. We only include functions that users could 'stumble upon' with
the right set of inputs.

## Related projects

This project was inspired by the Finder in Smalltalk, which does
something similar. There's
[a great demo video here](https://www.youtube.com/watch?v=HOuZyOKa91o#t=5m05s).

There are some other niche tools that take other approaches. For
example, [cant for Python](https://github.com/kootenpv/cant) tries
every function in scope (without a safety whitelist) to find
functionality.

## License

GPLv3.
