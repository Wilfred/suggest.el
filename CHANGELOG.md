# v0.4

Added bitwise functions `lsh`, `logand`, `logior`, `logxor`, and
`lognot`.

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
