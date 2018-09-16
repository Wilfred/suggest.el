;;; suggest.el --- suggest elisp functions that give the output requested  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.8
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (loop "1.3") (dash "2.13.0") (s "1.11.0") (f "0.18.2") (spinner "1.7.3"))
;; URL: https://github.com/Wilfred/suggest.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Suggest.el will find functions that give the output requested. It's
;; a great way of exploring list, string and arithmetic functions.

;;; Code:

(require 'dash)
(require 'seq)
(require 'loop)
(require 's)
(require 'f)
(require 'spinner)
(require 'subr-x)
(require 'cl-extra) ;; cl-prettyprint
(require 'cl-lib) ;; cl-incf, cl-evenp, cl-oddp

(defcustom suggest-insert-example-on-start t
  "If t, insert example data in suggest buffer, else don't."
  :group 'suggest
  :type 'boolean)

;; See also `cl--simple-funcs' and `cl--safe-funcs'.
(defvar suggest-functions
  (list
   ;; TODO: add funcall, apply and map?
   ;; Boolean functions
   #'not
   ;; Type predicates
   #'arrayp
   #'atom
   #'booleanp
   #'consp
   #'floatp
   #'functionp
   #'integerp
   #'listp
   #'numberp
   #'sequencep
   #'stringp
   #'symbolp
   ;; Sequence predicates
   #'seq-set-equal-p
   #'seq-empty-p
   ;; Built-in functions that access or examine lists.
   #'car
   #'cdr
   #'cadr
   #'cdar
   #'last
   #'cons
   #'nth
   #'nthcdr
   #'list
   #'length
   #'safe-length
   #'reverse
   #'remove
   #'remq
   #'append
   #'butlast
   ;; Built-in functions that create lists.
   #'make-list
   #'number-sequence
   ;; Sequence functions
   #'elt
   #'aref
   #'seq-subseq
   #'seq-drop
   #'seq-take
   #'seq-sort
   #'seq-reverse
   #'seq-concatenate
   #'seq-into
   #'seq-position
   #'seq-uniq
   #'seq-partition
   #'seq-intersection
   #'seq-difference
   ;; CL list functions.
   #'cl-first
   #'cl-second
   #'cl-third
   #'cl-list*
   ;; dash.el list functions.
   #'-non-nil
   #'-slice
   #'-take
   #'-take-last
   #'-drop
   #'-drop-last
   #'-select-by-indices
   #'-select-column
   #'-concat
   #'-flatten
   #'-replace
   #'-replace-first
   #'-insert-at
   #'-replace-at
   #'-remove-at
   #'-remove-at-indices
   #'-sum
   #'-product
   #'-min
   #'-max
   #'-is-prefix-p
   #'-is-suffix-p
   #'-is-infix-p
   #'-split-at
   #'-split-on
   #'-partition
   #'-partition-all
   #'-elem-index
   #'-elem-indices
   #'-union
   #'-difference
   #'-intersection
   #'-distinct
   #'-rotate
   #'-sort
   #'-repeat
   #'-cons*
   #'-snoc
   #'-interpose
   #'-interleave
   #'-zip
   #'-first-item
   #'-last-item
   #'-butlast
   ;; alist functions
   #'assoc
   #'alist-get
   ;; plist functions
   #'plist-get
   #'lax-plist-get
   #'plist-member
   ;; hash tables
   #'gethash
   #'hash-table-keys
   #'hash-table-values
   ;; vectors
   ;; TODO: there must be more worth using
   #'vector
   #'vconcat
   ;; Arithmetic
   #'+
   #'-
   #'*
   #'/
   #'%
   #'mod
   #'max
   #'min
   #'ash
   #'lsh
   #'log
   #'expt
   #'sqrt
   #'abs
   #'float
   #'round
   #'truncate
   #'ceiling
   #'fceiling
   #'ffloor
   #'fround
   #'ftruncate
   #'1+
   #'1-
   #'cl-evenp
   #'natnump
   #'cl-oddp
   #'zerop
   ;; Logical operators
   #'lsh
   #'logand
   #'logior
   #'logxor
   #'lognot
   ;; Strings
   #'string
   #'make-string
   #'upcase
   #'downcase
   #'substring
   #'concat
   #'split-string
   #'capitalize
   #'replace-regexp-in-string
   #'format
   #'string-join
   #'string-prefix-p
   #'string-suffix-p
   #'string-remove-prefix
   #'string-remove-suffix
   #'prin1-to-string
   ;; Quoting strings
   #'shell-quote-argument
   #'regexp-quote
   ;; s.el string functions
   #'s-trim
   #'s-trim-left
   #'s-trim-right
   #'s-pad-left
   #'s-pad-right
   #'s-chomp
   #'s-collapse-whitespace
   #'s-word-wrap
   #'s-left
   #'s-right
   #'s-chop-suffix
   #'s-chop-suffixes
   #'s-chop-prefix
   #'s-chop-prefixes
   #'s-shared-start
   #'s-shared-end
   #'s-truncate
   #'s-repeat
   #'s-concat
   #'s-prepend
   #'s-append
   #'s-lines
   #'s-split
   #'s-join
   #'s-ends-with-p
   #'s-starts-with-p
   #'s-contains-p
   #'s-replace
   #'s-capitalize
   #'s-index-of
   #'s-reverse
   #'s-count-matches
   #'s-split-words
   #'s-wrap
   ;; Symbols
   #'symbol-name
   #'symbol-value
   #'symbol-file
   #'intern
   ;; Converting between types
   #'string-to-list
   #'string-to-number
   #'string-to-char
   #'number-to-string
   #'char-to-string
   ;; Paths
   #'file-name-as-directory
   #'file-name-base
   #'file-name-directory
   #'file-name-nondirectory
   #'file-name-extension
   #'expand-file-name
   #'abbreviate-file-name
   #'directory-file-name
   ;; Paths with f.el
   #'f-join
   #'f-split
   #'f-filename
   #'f-parent
   #'f-common-parent
   #'f-ext
   #'f-no-ext
   #'f-base
   #'f-short
   #'f-long
   #'f-canonical
   #'f-slash
   #'f-depth
   #'f-relative
   ;; These are not pure, but still safe:
   #'f-files
   #'f-directories
   #'f-entries
   ;; Keyboard codes
   #'kbd
   #'key-description
   ;; Generic functions
   #'identity
   #'ignore
   )
  "Functions that suggest will consider.

These functions must not produce side effects, and must not be
higher order functions.

Side effects are obviously bad: we don't want to call
`delete-file' with arbitrary strings!

Higher order functions are any functions that call `funcall' or
`apply'. These are not safe to call with arbitrary symbols, but
see `suggest-funcall-functions'.

The best functions for examples generally take a small number of
arguments, and no arguments are functions. For other functions,
the likelihood of users discovering them is too low.

Likewise, we avoid predicates of one argument, as those generally
need multiple examples to ensure they do what the user wants.

See also `suggest-extra-args'.")

(defvar suggest-funcall-functions
  (list
   ;; Higher order list functions.
   #'mapcar
   ;; When called with a symbol, read will call it.
   #'read
   ;; dash.el higher order list functions.
   #'-map
   #'-mapcat
   ;; dash.el folding/unfolding
   #'-reduce
   #'-reduce-r
   #'-iterate
   )
  "Pure functions that may call `funcall'. We will consider
consider these, but only with arguments that are known to be safe." )

(defvar suggest-extra-args
  (list
   ;; There's no special values for `list', and it produces silly
   ;; results when we add values.
   #'list '()
   ;; Similarly, we can use nil when building a list, but otherwise
   ;; we're just building an irrelevant list if we use the default
   ;; values.
   #'cons '(nil)
   #'-cons* '(nil)
   #'-snoc '(nil)
   #'append '(nil)
   #'vector '(nil)
   ;; `format' has specific formatting strings that are worth trying.
   #'format '("%d" "%o" "%x" "%X" "%e" "%c" "%f" "%s" "%S")
   ;; `-iterate' is great for building incrementing/decrementing lists.
   ;; (an alternative to `number-sequence').
   #'-iterate '(1+ 1-)
   ;; Lists can be sorted in a variety of ways.
   #'-sort '(< > string< string>)
   #'seq-sort '(< > string< string>)
   ;; Sequences of type.
   #'seq-concatenate '(vector string list)
   #'seq-into '(vector string list)
   ;; For indexing functions, just use non-negative numbers. This
   ;; avoids confusing results like (last nil 5) => nil.
   #'elt '(0 1 2)
   #'nth '(0 1 2)
   #'nthcdr '(0 1 2)
   #'last '(0 1 2)
   #'-drop '(0 1 2)
   #'-drop-last '(0 1 2)
   #'-take '(0 1 2)
   ;; For functions that look up a value, don't supply any extra
   ;; arguments.
   #'plist-member '()
   #'assoc '()
   ;; Likewise for comparisons, there's no interesting extra value we can offer.
   #'-is-suffix-p '()
   #'-is-prefix-p '()
   #'-is-infix-p '()
   ;; Joining strings is commonly done with spaces or newlines.
   #'string-join '("\n" " ")            ; default is "" already.
   #'s-join '("" "\n" " ")
   ;; Remove has some weird behaviours with t: (remove 'foo t) => t.
   ;; Only use nil as an extra value, so we can discover remove as an
   ;; alternative to `-non-nil'.
   #'remove '(nil)
   ;; mapcar with identity is a fun way to convert sequences (strings,
   ;; vectors) to lists.
   #'mapcar '(identity)
   ;; These common values often set flags in interesting
   ;; ways.
   t '(nil t -1 0 1 2))
  "Some functions work best with a special extra argument.

This plist associates functions with particular arguments that
produce good results. If a function isn't explicitly mentioned,
we look up `t' instead.")

(defun suggest--unsafe-p (fn args)
  "Is FN unsafe to call with ARGS?

Safety here means that we:

* don't have any side effects, and
* don't crash Emacs."
  (or
   ;; Due to Emacs bug #25684, string functions that call
   ;; caseify_object in casefiddle.c cause Emacs to segfault when
   ;; given negative integers.
   (and (memq fn '(upcase downcase capitalize upcase-initials))
        (consp args)
        (null (cdr args))
        (integerp (car args))
        (< (car args) 0))
   ;; If `read' is called with nil or t, it prompts interactively.
   (and (eq fn 'read)
        (member args '(nil (nil) (t))))
   ;; Work around https://github.com/Wilfred/suggest.el/issues/37 on
   ;; Emacs 24, where it's possible to crash `read' with a list.
   (and (eq fn 'read)
        (eq emacs-major-version 24)
        (consp (car args)))
   ;; Work around https://github.com/magnars/dash.el/issues/241
   (and (memq fn '(-interleave -zip))
        (null args))
   (and (memq fn suggest-funcall-functions) ;
        ;; TODO: what about circular lists?
        ;;
        ;; Does apply even handle that nicely? It looks like apply
        ;; tries to get the length of the list and hangs until C-g.
        (format-proper-list-p args)
        (--any (or
                ;; Don't call any higher order functions with symbols that
                ;; aren't known to be safe.
                (and (symbolp it) (not (memq it suggest-functions)))
                ;; Don't allow callable objects (interpreted or
                ;; byte-compiled function objects).
                (and (not (symbolp it)) (functionp it)))
               args))))

(defun suggest--safe-p (fn args)
  "Is FN safe to call with ARGS?"
  (not (suggest--unsafe-p fn args)))

(defface suggest-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for headings."
  :group 'suggest)

(defvar suggest--inputs-heading ";; Inputs (one per line):")
(defvar suggest--outputs-heading ";; Desired output:")
(defvar suggest--results-heading ";; Suggestions:")

(defun suggest--insert-heading (text)
  "Highlight TEXT as a heading and insert in the current buffer."
  ;; Make a note of where the heading starts.
  (let ((excluding-last (substring text 0 (1- (length text))))
        (last-char (substring text (1- (length text))))
        (start (point))
        end)
    ;; Insert the heading, ensuring it's not editable,
    (insert (propertize excluding-last
                        'read-only t))
    ;; but allow users to type immediately after the heading.
    (insert (propertize last-char
                        'read-only t
                        'rear-nonsticky t))
    ;; Point is now at the end of the heading, save that position.
    (setq end (point))
    ;; Start the overlay after the ";; " bit.
    (let ((overlay (make-overlay (+ 3 start) end)))
      ;; Highlight the text in the heading.
      (overlay-put overlay 'face 'suggest-heading)))
  (insert "\n"))

(defun suggest--on-heading-p ()
  "Return t if point is on a heading."
  (get-char-property (point) 'read-only))

(defun suggest--raw-inputs ()
  "Read the input lines in the current suggestion buffer."
  (let ((headings-seen 0)
        (raw-inputs nil))
    (loop-for-each-line
      ;; Make a note of when we've passed the inputs heading.
      (when (and (suggest--on-heading-p))
        (cl-incf headings-seen)
        (if (equal headings-seen 2)
            ;; Stop once we reach the outputs.
            (loop-return (nreverse raw-inputs))
          (loop-continue)))
      ;; Skip over empty lines.
      (when (equal it "")
        (loop-continue))
      (push (substring-no-properties it) raw-inputs))))

;; TODO: check that there's only one line of output, or prevent
;; multiple lines being entered.
(defun suggest--raw-output ()
  "Read the output line in the current suggestion buffer."
  (save-excursion
    ;; Move past the 'desired output' heading.
    (suggest--nth-heading 2)
    (forward-line 1)
    ;; Skip blank lines.
    (while (looking-at "\n")
      (forward-line 1))
    ;; Return the current line.
    (buffer-substring (point) (line-end-position))))

(defun suggest--keybinding (command keymap)
  "Find the keybinding for COMMAND in KEYMAP."
  (where-is-internal command keymap t))

(defun suggest--insert-section-break ()
  "Insert section break."
  (insert "\n\n"))

;;;###autoload
(defun suggest ()
  "Open a Suggest buffer that provides suggestions for the inputs
and outputs given."
  (interactive)
  (let ((buf (get-buffer-create "*suggest*"))
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (switch-to-buffer buf)
    (erase-buffer)
    (suggest-mode)

    (suggest--insert-heading suggest--inputs-heading)
    (when suggest-insert-example-on-start
      (insert "1\n2"))

    (suggest--insert-section-break)

    (suggest--insert-heading suggest--outputs-heading)
    (when suggest-insert-example-on-start
      (insert "3"))

    (suggest--insert-section-break)

    (suggest--insert-heading suggest--results-heading)
    ;; Populate the suggestions for 1, 2 => 3
    (when suggest-insert-example-on-start
      (suggest-update))

    ;; Put point on the first input.
    (suggest--nth-heading 1)
    (forward-line 1))
  (add-hook 'first-change-hook
            (lambda () (suggest--update-needed t))
            nil t))

(defun suggest--nth-heading (n)
  "Move point to Nth heading in the current *suggest* buffer.
N counts from 1."
  (goto-char (point-min))
  (let ((headings-seen 0))
    (while (< headings-seen n)
      (when (suggest--on-heading-p)
        (cl-incf headings-seen))
      (forward-line 1)))
  (forward-line -1))

(defun suggest--write-suggestions-string (text)
  "Write TEXT to the suggestion section."
  (let ((inhibit-read-only t))
    (save-excursion
      ;; Move to the first line of the results.
      (suggest--nth-heading 3)
      (forward-line 1)
      ;; Remove the current suggestions text.
      (delete-region (point) (point-max))
      ;; Insert the text, ensuring it can't be edited.
      (insert (propertize text 'read-only t)))))

(defun suggest--join-func-output (formatted-call formatted-value)
  "Combine strings FORMATTED-CALL and FORMATTED-VALUE and indent."
  (let* ((prefixed-lines
          ;; Put a ;=> before FORMATTED-VALUE.
          (--map-indexed
           (if (zerop it-index) (concat ";=> " it) (concat ";   " it))
           (s-lines formatted-value)))
         ;; A string of spaces the same length as FORMATTED-CALL.
         (matching-spaces (s-repeat (length formatted-call) " "))
         ;; If FORMATTED-VALUE is a multiline string, indent
         ;; subsequent lines.
         (formatted-lines
          (--map-indexed
           (if (zerop it-index)
               (format "%s %s" formatted-call it)
             (format "%s %s" matching-spaces it))
           prefixed-lines)))
    (s-join "\n" formatted-lines)))

(defun suggest--format-suggestion (suggestion raw-output)
  "Format SUGGESTION as a lisp expression returning RAW-OUTPUT.
RAW-OUTPUT is a string, so we can distinguish literals,
e.g. decimal 16 from hex #x10."
  (let* ((formatted-call "")
         (func-output (plist-get suggestion :output))
         (desired-output (read raw-output))
         (formatted-output
          (if (and (numberp desired-output)
                   (eq (type-of desired-output) (type-of func-output)))
              ;; If the user typed a number literal #x10, and we
              ;; didn't change type (e.g. 1.0 vs 1), we want to show
              ;; the same literal in the output.
              raw-output
            ;; Otherwise, show the actual function output.
            (pp-to-string func-output))))
    ;; Build up a string "(func1 (func2 ... literal-inputs))"
    (let ((funcs (plist-get suggestion :funcs))
          (literals (plist-get suggestion :literals)))
      (dolist (func funcs)
        (let ((func-sym (plist-get func :sym))
              (variadic-p (plist-get func :variadic-p)))
          (if variadic-p
              (setq formatted-call
                    (format "%s(apply #'%s " formatted-call func-sym))
            (setq formatted-call
                  (format "%s(%s " formatted-call func-sym)))))
      (setq formatted-call
            (format "%s%s" formatted-call
                    (s-join " " literals)))
      (setq formatted-call
            (concat formatted-call (s-repeat (length funcs) ")"))))
    (suggest--join-func-output formatted-call formatted-output)))

(defun suggest--write-suggestions (suggestions raw-output)
  "Write SUGGESTIONS to the current *suggest* buffer.
SUGGESTIONS is a list of forms."
  (->> suggestions
       (--map (suggest--format-suggestion it raw-output))
       (s-join "\n")
       (suggest--write-suggestions-string)))

(defun suggest--read (form)
  "Read FORM, but don't open a debugger on errors."
  (condition-case err
      (read form)
    (error (user-error
            "Could not parse %s: %s" form err))))

(defun suggest--eval (form)
  "Eval FORM, but don't open a debugger on errors."
  (condition-case err
      (eval form)
    (error (user-error
            "Could not eval %s: %s" form err))))

;; TODO: this would be a good match for dash.el.
(defun suggest--permutations (lst)
  "Return a list of all possible orderings of list LST."
  (cl-case (length lst)
    (0 nil)
    (1 (list lst))
    (t
     ;; TODO: this is ugly.
     ;; TODO: extract an accumulate macro?
     (let ((permutations nil))
       (--each-indexed lst
         (let* ((element it)
                (remainder (-remove-at it-index lst))
                (remainder-perms (suggest--permutations remainder)))
           (--each remainder-perms (push (cons element it) permutations))))
       (nreverse permutations)))))

(defconst suggest--search-depth 4
  "The maximum number of nested function calls to try.
This tends to impact performance for values where many functions
could work, especially numbers.")

(defconst suggest--max-possibilities 20
  "The maximum number of possibilities to return.
This has a major impact on performance, and later possibilities
tend to be progressively more silly.")

(defconst suggest--max-intermediates 200)

(defconst suggest--max-per-value 3)

(defsubst suggest--classify-output (inputs func-output target-output)
  "Classify FUNC-OUTPUT so we can decide whether we should keep it."
  (cond
   ((or
     (and (numberp func-output)
          (numberp target-output)
          (= func-output target-output))
     (equal func-output target-output))
    'match)
   ;; If the function gave us nil, we're not going to
   ;; find any interesting values by further exploring
   ;; this value.
   ((null func-output)
    'ignore)
   ;; If the function gave us the same target-output as our
   ;; input, don't bother exploring further. Too many
   ;; functions return the input if they can't do
   ;; anything with it.
   ((and (equal (length inputs) 1)
         (equal (-first-item inputs) func-output))
    'ignore)
   ;; The function returned a different result to what
   ;; we wanted, but might be worth exploring further.
   (t
    'different)))

(defsubst suggest--call (func values literals &optional variadic-p)
  "Call FUNC with VALUES, ignoring all errors.
If FUNC returns a value, return a plist (:output ...). Returns
nil otherwise."
  (when (suggest--safe-p func (if variadic-p
                                  (car values)
                                values))
    (let ((default-directory "/")
          (file-name-handler-alist nil)
          func-output func-success)
      (ignore-errors
        (setq func-output
              (if variadic-p
                  (apply func (car values))
                (apply func values)))
        (setq func-success t))
      (when func-success
        (list :output func-output
              :variadic-p variadic-p
              :literals literals)))))

(defun suggest--unread (value)
  "Convert VALUE to a string that can be read to obtain VALUE.
This is primarily for quoting symbols."
  (cond
   ((consp value)
    (format "'%S" value))
   ((functionp value)
    (format "#'%s" value))
   ((and
     (symbolp value)
     (not (keywordp value))
     (not (eq value nil))
     (not (eq value t)))
    (format "'%s" value))
   (t
    (s-trim (pp-to-string value)))))

(defsubst suggest--try-call (iteration func input-values input-literals)
  "Try to call FUNC with arguments INPUT-VALUES, and return a list of outputs"
  (let (outputs)
    ;; See if (func value1 value2...) gives us a value.
    (-when-let (result (suggest--call func input-values input-literals))
      (push result outputs))

    ;; See if (apply func input-values) gives us a value.
    (when (and (eq (length input-values) 1) (listp (car input-values)))
      (-when-let (result (suggest--call func input-values input-literals t))
        (push result outputs)))

    ;; See if (func COMMON-CONSTANT value1 value2...) gives us a value.
    (when (zerop iteration)
      (dolist (extra-arg (if (plist-member suggest-extra-args func)
                             (plist-get suggest-extra-args func)
                           (plist-get suggest-extra-args t)))
        (dolist (position '(after before))
          (let ((args (if (eq position 'before)
                          (cons extra-arg input-values)
                        (-snoc input-values extra-arg)))
                (literals (if (eq position 'before)
                              (cons (suggest--unread extra-arg) input-literals)
                            (-snoc input-literals (suggest--unread extra-arg)))))
            (-when-let (result (suggest--call func args literals))
              (push result outputs))))))
    ;; Return results in ascending order of preference, so we prefer
    ;; (+ 1 2) over (+ 0 1 2).
    (nreverse outputs)))

(defmacro suggest--dolist-catch (var-val-tag &rest body)
  "Loop over a list, terminating early if TAG is thrown.
Evaluate BODY with VAR bound to each car from LIST, in turn.

\(fn (VAR LIST TAG) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp var-val-tag)
    (signal 'wrong-type-argument (list 'consp var-val-tag)))
  (unless (= (length var-val-tag) 3)
    (signal 'wrong-number-of-arguments (list 3 (length var-val-tag))))
  (-let [(var val tag) var-val-tag]
    `(catch ,tag
       (dolist (,var ,val)
         ,@body))))

(defun suggest--possibilities (input-literals input-values output)
  "Return a list of possibilities for these INPUTS-VALUES and OUTPUT.
Each possbility form uses INPUT-LITERALS so we show variables rather
than their values."
  (let (possibilities
        (possibilities-count 0)
        this-iteration
        intermediates
        (intermediates-count 0)
        (value-occurrences (make-hash-table :test #'equal))
        (funcs (append suggest-functions suggest-funcall-functions))
        ;; Since we incrementally build a huge list of results,
        ;; garbage collection does a load of work but doesn't find
        ;; much to collect. Disable GC, as it significantly improves
        ;; performance.
        (gc-cons-threshold 50000000))
    ;; Setup: no function calls, all permutations of our inputs.
    (setq this-iteration
          (-map (-lambda ((values . literals))
                  (list :funcs nil :values values :literals literals))
                ;; Only consider unique permutations.
                (-distinct
                 (-zip-pair (suggest--permutations input-values)
                            (suggest--permutations input-literals)))))
    (catch 'done
      (dotimes (iteration suggest--search-depth)
        ;; We need to call redisplay so the spinner keeps rotating
        ;; as we search.
        (redisplay)
        (suggest--dolist-catch (func funcs 'done-iteration)
          (suggest--dolist-catch (item this-iteration 'done-func)
            (let ((literals (plist-get item :literals))
                  (values (plist-get item :values))
                  (funcs (plist-get item :funcs)))
              ;; Try to call the function, then classify its return values.
              (dolist (func-result (suggest--try-call iteration func values literals))
                (let ((func-output (plist-get func-result :output)))
                  (cl-case (suggest--classify-output values func-output output)
                    ;; The function gave us the output we wanted, just save it.
                    ('match
                     (push
                      (list :funcs (cons (list :sym func
                                               :variadic-p (plist-get func-result :variadic-p))
                                         funcs)
                            :literals (plist-get func-result :literals)
                            :output func-output)
                      possibilities)
                     (cl-incf possibilities-count)
                     (when (>= possibilities-count suggest--max-possibilities)
                       (throw 'done nil))

                     ;; If we're on the first iteration, we're just
                     ;; searching all input permutations. Don't try any
                     ;; other permutations, or we end up showing e.g. both
                     ;; (+ 2 3) and (+ 3 2).
                     (when (zerop iteration)
                       (throw 'done-func nil)))
                    ;; The function returned a different result to what
                    ;; we wanted. Build a list of these values so we
                    ;; can explore them.
                    ('different
                     (when (and
                            (< intermediates-count suggest--max-intermediates)
                            (< (gethash func-output value-occurrences 0)
                               suggest--max-per-value))
                       (puthash
                        func-output
                        (1+ (gethash func-output value-occurrences 0))
                        value-occurrences)
                       (cl-incf intermediates-count)
                       (push
                        (list :funcs (cons (list :sym func
                                                 :variadic-p (plist-get output :variadic-p))
                                           funcs)
                              :literals (plist-get func-result :literals)
                              :values (list func-output))
                        intermediates)))))))))

        (setq this-iteration intermediates)
        (setq intermediates nil)
        (setq intermediates-count 0)))
    possibilities))

(defun suggest--cmp-relevance (pos1 pos2)
  "Compare two possibilities such that the more relevant result
  is smaller."
  (let ((pos1-func-count (length (plist-get pos1 :funcs)))
        (pos2-func-count (length (plist-get pos2 :funcs)))
        (pos1-arg-count (length (plist-get pos1 :literals)))
        (pos2-arg-count (length (plist-get pos2 :literals)))
        (pos1-apply-count (length (--filter (plist-get it :variadic-p)
                                            (plist-get pos1 :funcs))))
        (pos2-apply-count (length (--filter (plist-get it :variadic-p)
                                            (plist-get pos2 :funcs)))))
    (cond
     ;; If we have the same number of function calls, with the same
     ;; number of arguments, prefer functions with shorter names. This
     ;; is a dumb but surprisingly effective heuristic.
     ((and
       (= pos1-func-count pos2-func-count)
       (= pos1-arg-count pos2-arg-count)
       (= pos1-apply-count pos2-apply-count))
      (let ((join-names (lambda (pos)
                          (->> (plist-get pos :funcs)
                               (--map (plist-get it :sym))
                               (-map #'symbol-name)
                               (apply #'concat)))))
        (< (length (funcall join-names pos1)) (length (funcall join-names pos2)))))

     ;; Prefer direct function calls to using apply.
     ((and
       (= pos1-func-count pos2-func-count)
       (= pos1-arg-count pos2-arg-count))
      (< pos1-apply-count pos2-apply-count))

     ;; Prefer calls that don't have extra arguments, so prefer (1+ 1)
     ;; over (+ 1 1).
     ((= pos1-func-count pos2-func-count)
      (< pos1-arg-count pos2-arg-count))

     ;; Prefer fewer function calls over all else.
     (t
      (< (length (plist-get pos1 :funcs)) (length (plist-get pos2 :funcs)))))))

(defvar suggest--spinner nil)

;;;###autoload
(defun suggest-update ()
  "Update the suggestions according to the latest inputs/output given."
  (interactive)
  (setq suggest--spinner (spinner-create 'progress-bar t))
  (spinner-start suggest--spinner)

  (unwind-protect
      ;; TODO: error on multiple inputs on one line.
      (let* ((raw-inputs (suggest--raw-inputs))
             (inputs (--map (suggest--eval (suggest--read it)) raw-inputs))
             (raw-output (suggest--raw-output))
             (desired-output (suggest--eval (suggest--read raw-output)))
             (possibilities
              (suggest--possibilities raw-inputs inputs desired-output)))
        ;; Sort, and take the top 5 most relevant results.
        (setq possibilities
              (-take 5
                     (-sort #'suggest--cmp-relevance possibilities)))

        (if possibilities
            (suggest--write-suggestions possibilities raw-output)
          (suggest--write-suggestions-string ";; No matches found.")))
    (setq suggest--spinner nil))
  (suggest--update-needed nil)
  (set-buffer-modified-p nil))

(define-derived-mode suggest-mode emacs-lisp-mode
  '("Suggest" (:eval (spinner-print suggest--spinner)))
  "A major mode for finding functions that provide the output requested.")

(define-key suggest-mode-map (kbd "C-c C-c") #'suggest-update)

(defun suggest--update-needed (update-needed)
  "Update the suggestions heading to say whether we need
the user to call `suggest-update'."
  (save-excursion
    (suggest--nth-heading 3)
    (let ((inhibit-read-only t))
      (delete-region (point) (line-end-position))
      (if update-needed
          (suggest--insert-heading
           (format ";; Suggestions (press %s to update):"
                   (key-description
                    (suggest--keybinding #'suggest-update suggest-mode-map))))
        (suggest--insert-heading suggest--results-heading)))))

(provide 'suggest)
;;; suggest.el ends here
