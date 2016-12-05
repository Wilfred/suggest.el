# v0.3

Added `f-relative`, `last`, `safe-length`, `remove`, `remq`,
`number-sequence`, `alist-get`, `kbd` and `key-description`.

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
