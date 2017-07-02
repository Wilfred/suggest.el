# suggest.el

***did you mean cadr?***

[![Build Status](https://travis-ci.org/Wilfred/suggest.el.svg?branch=master)](https://travis-ci.org/Wilfred/suggest.el)
[![Coverage Status](https://coveralls.io/repos/github/Wilfred/suggest.el/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/suggest.el?branch=master)
[![MELPA](http://melpa.org/packages/suggest-badge.svg)](http://melpa.org/#/suggest)

suggest.el is an Emacs package for discovering elisp functions based
on examples. See my blog post:
[Example Driven Development](http://www.wilfred.me.uk/blog/2016/07/30/example-driven-development/).

![suggest](suggest_screenshot.png)

## Examples

suggest.el knows many string functions:

``` emacs-lisp
;; Inputs (one per line):
"foo bar"

;; Desired output:
"Foo Bar"

;; Suggestions:
(capitalize "foo bar") ;=> "Foo Bar"
```

suggest.el can also help you find tricky dash.el functions:

``` emacs-lisp
;; Inputs (one per line):
(list 'a 'b 'c 'd)
'c

;; Desired output:
2

;; Suggestions:
(-elem-index 'c (list 'a 'b 'c 'd)) ;=> 2
```

suggest.el is particularly handy for path manipulation, using both
built-in functions as well as f.el:

``` emacs-lisp
;; Inputs (one per line):
"/foo/bar/baz.txt"

;; Desired output:
"baz.txt"

;; Suggestions:
(file-name-nondirectory "/foo/bar/baz.txt") ;=> "baz.txt"
(f-filename "/foo/bar/baz.txt") ;=> "baz.txt"
```

It can even suggest calling functions with `apply`:

``` emacs-lisp
;; Inputs (one per line):
'(1 2 3 4 5)

;; Desired output:
15

;; Suggestions:
(-sum '(1 2 3 4 5)) ;=> 15
(apply #'+ '(1 2 3 4 5)) ;=> 15
```

It can also suggest composing functions:

``` emacs-lisp
;; Inputs (one per line):
'(a b c)

;; Desired output:
'c

;; Suggestions:
(cadr (cdr '(a b c))) ;=> 'c
(car (last '(a b c))) ;=> 'c
(cl-third '(a b c)) ;=> 'c
```

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

barliman takes this idea of synthesis much, much further for
Scheme. There's an
[incredible demo video here](https://www.youtube.com/watch?v=er_lLvkklsk).

## License

GPLv3.
