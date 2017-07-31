# suggest.el

***did you mean cadr?***

[![Build Status](https://travis-ci.org/Wilfred/suggest.el.svg?branch=master)](https://travis-ci.org/Wilfred/suggest.el)
[![Coverage Status](https://coveralls.io/repos/github/Wilfred/suggest.el/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/suggest.el?branch=master)
[![MELPA](http://melpa.org/packages/suggest-badge.svg)](http://melpa.org/#/suggest)

suggest.el is an Emacs package for discovering elisp functions based
on examples. You supply an example input and output, and it makes suggestions.

Interested readers may enjoy my blog post:
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

## License

GPLv3.

## Related projects

Program synthesis or inductive programming is a computer science
research topic, with a range of tools taking very different approaches
to generate code.

### Smalltalk: Finder

This project was inspired by the Finder in Smalltalk, which does
something similar. There's
[a great demo video here](https://www.youtube.com/watch?v=HOuZyOKa91o#t=5m05s).

You give a list of values (inputs followed by an output):

```
3. 4. 7.
```

The Finder iterates over all possible calls (with arguments in any
order) to a list of known safe messages, and returns suggestions:

```
3 + 4 --> 7
3 bitOr: 4 --> 7
3 bitXor: 4 --> 7
3 | 4 --> 7
```

This only returns single messages that meet the requirements, no
nesting (e.g. it won't find `'(data1 reversed asUppercase)'` from
`'foo'. 'OOF'`).

You can also supply multiple examples, using expressions as inputs:

```
MethodFinder methodFor: { {'29 Apr 1999' asDate}. 'Thursday'.
		{'30 Apr 1999' asDate}. 'Friday' }.
```

This returns `'(data1 weekday)'`.

Amusingly, it will sometimes find `'(data1 shuffled)'` from
`'fo'. 'of'.`, which is a random sort.

### Python: cant

There are some other niche tools that take other approaches. For
example, [cant for Python](https://github.com/kootenpv/cant) tries
every function in scope (without a safety whitelist) to find
functionality.

### Scheme: barliman

barliman takes this idea of synthesis much, much further for
Scheme. There's an
[incredible demo video here](https://www.youtube.com/watch?v=er_lLvkklsk).

### C-like: sketch

[sketch](https://bitbucket.org/gatoatigrado/sketch-frontend/wiki/Home)
allows the user to specify value placeholders in code
([examples](https://bitbucket.org/gatoatigrado/sketch-frontend/wiki/Gallery)).

```
harness void doubleSketch(int x){
  int t = x * ??;
  assert t == x + x;
}
```

Generally you provide the structure of the code.

```
bit[W] firstLeadingZeroSketch (bit[W] x) implements firstLeadingZero {	
	return !(x + ??) & (x + ??); 
}
```

Or you can specify a set of operators for sketch to explore. Each line
here is an assignment to `x` or `tmp` of an expression that may
contain `!` `&` `+` with `x`, `tmp` or a constant as arguments.

```
bit[W] firstLeadingZeroSketch (bit[W] x) implements firstLeadingZero {	
	bit[W] tmp=0;
       {| x | tmp |} = {| (!)?((x | tmp) (& | +) (x | tmp | ??)) |};
       {| x | tmp |} = {| (!)?((x | tmp) (& | +) (x | tmp | ??)) |};
       {| x | tmp |} = {| (!)?((x | tmp) (& | +) (x | tmp | ??)) |};
       return tmp;
}
```

Constraints are fed to a SAT solver, then sketch finds a solution. The
output can be converted to C.

### Liquid Haskell: Synquid

[Synquid](https://bitbucket.org/nadiapolikarpova/synquid) (the first
half of
[this Microsoft Research talk](https://www.youtube.com/watch?v=Q-3tcbUyF34) gives
a good overview) is a program synthesis tool leveraging refinement
types.

The user provides the type of the function they want to generate, and
a collection of 'components', the building blocks that Synquid tries
to combine.

Synquid then lazily generates ASTs and type checks them. This allows
it to prune the search tree by ignoring program structures that are
never valid types.

(This is the extent of my understanding: I have probably
oversimplified.)

Impressively, Synquid can generate recursive functions.
