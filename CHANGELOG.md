# v0.2

Added some additional functions: `elt`, `butlast`, `make-list`,
`lax-plist-get`, `abbreviate-file-name`, `replace-regexp-in-string`,
`string`, `make-string`, `split-string`, `string-to-list`, `vconcat`,
`s-right`, `s-pad-left`, `s-pad-right`, `string-to-number`,
`string-to-char`, `number-to-string`, `char-to-string`,
`file-name-nondirectory`.

Made more the of the `*suggest*` buffer editable, as users often press
RET at the end of a heading line.

We can now suggest `(apply #'some-func some-list)` too. Try an input
of `'(2 3 4)` and an output of `24`.

Fixed an issue displaying function outputs that spread over multiple
lines.

# v0.1

Initial release.
