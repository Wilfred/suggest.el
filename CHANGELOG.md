# v0.6

Added mapping functions `mapcar`, `-map` and `-mapcat`.

# v0.5

Suggest.el now only explores unique permutations of
inputs. Previously, if the user provided `2 2 => 4` we would try the
inputs reversed. This was slower and led to duplicate suggestions.

Suggest.el now returns the same results regardless of the value of
`default-directory` and `file-name-handler-alist`. This fixes a bug
where suggest.el would try to make tramp connections from URLs
([#32](https://github.com/Wilfred/suggest.el/issues/32)).

Added string function `s-truncate`.

Added parsing function `read`.

Improved sorting of suggestions to penalise `apply` relative to direct
function calls.

Added formatting function `format`.

Defined a variable `suggest-extra-args` so suggest.el may consider
additional function-specific arguments.

Added functions `-reduce`, `-reduce-r` and `-iterate`.

# v0.4

The search algorithm is now smarter at avoiding previoiusly-seen
values, resulting in a larger search space being explored. You may see
additional suggestions.

suggest.el may now suggest additional arguments, e.g. it can suggest
`(nth 0 '(1 2 3))` from just `'(1 2 3)` as an input.

Added bitwise functions `lsh`, `logand`, `logior`, `logxor`, and
`lognot`.

Added string function `s-wrap`.

Fixed crash on repeated `M-x suggest`.

# v0.3

Suggest.el can now suggest sequences of function calls, e.g.
`(cdr (cdr foo))`.

Suggest.el now avoids calling some string functions with arguments
that are known to segfault Emacs
([#16](https://github.com/Wilfred/suggest.el/issues/16)).

Added `s-chop-prefix`, `s-chop-prefixes`, `s-chop-suffixes`,
`f-relative`, `last`, `safe-length`, `remove`, `remq`,
`number-sequence`, `alist-get`, `ignore`, `symbol-file`,
`regexp-quote`, `shell-quote-argument`, `kbd`, `key-description`
and `intern`.

# v0.2

Added examples to the README to help new users understand why they'd
use suggest.el.

Added some additional functions: `elt`, `butlast`, `make-list`,
`lax-plist-get`, `abbreviate-file-name`, `replace-regexp-in-string`,
`string`, `make-string`, `split-string`, `string-to-list`, `vconcat`,
`s-right`, `s-pad-left`, `s-pad-right`, `string-to-number`,
`string-to-char`, `number-to-string`, `char-to-string`,
`file-name-nondirectory`, `directory-file-name`, `aref`.

Made more the of the `*suggest*` buffer editable, as users often press
RET at the end of a heading line.

We can now suggest `(apply #'some-func some-list)` too. Try an input
of `'(2 3 4)` and an output of `24`.

Fixed an issue displaying function outputs that spread over multiple
lines.

# v0.1

Initial release.
