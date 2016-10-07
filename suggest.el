;;; suggest.el --- suggest elisp functions that give the output requested  -*- lexical-binding: t; -*-

;; Copyright (C) 2016

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.2
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (loop "1.3") (dash "2.13.0") (s "1.11.0") (f "0.18.2"))

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
(require 'loop)
(require 's)
(require 'f)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib)) ;; cl-incf

;; TODO: add (format %s _) somehow
;; TODO: add #'read, but don't prompt for input when the example is nil.
(defvar suggest-functions
  (list
   #'identity
   ;; Built-in list functions.
   #'car
   #'cdr
   #'cadr
   #'cdar
   #'cons
   #'nth
   #'list
   #'length
   #'reverse
   #'append
   #'butlast
   #'make-list
   ;; Sequence functions
   #'elt
   #'aref
   ;; CL list functions.
   #'cl-first
   #'cl-second
   #'cl-third
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
   #'s-shared-start
   #'s-shared-end
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
   ;; Symbols
   #'symbol-name
   #'symbol-value
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
   ;; These are not pure, but still safe:
   #'f-files
   #'f-directories
   #'f-entries
   )
  "Functions that suggest will consider.
These functions must not produce side effects.

The best functions for examples generally take a small number of
arguments, and no arguments are functions. For other functions,
the likelihood of users discovering them is too low.

Likewise, we avoid predicates of one argument, as those generally
need multiple examples to ensure they do what the user wants.")

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
      (overlay-put overlay 'face 'suggest-heading))))

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
      (push it raw-inputs))))

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
    (buffer-substring (point)
                      (progn (move-end-of-line nil) (point)))))

(defun suggest--keybinding (command keymap)
  "Find the keybinding for COMMAND in KEYMAP."
  (car (where-is-internal command keymap)))

;;;###autoload
(defun suggest ()
  "Open a Suggest buffer that provides suggestions for the inputs
and outputs given."
  (interactive)
  (let ((buf (get-buffer-create "*suggest*")))
    (switch-to-buffer buf)
    (erase-buffer)
    (suggest-mode)
    (let ((inhibit-read-only t))
      (suggest--insert-heading suggest--inputs-heading)
      (insert "\n1\n2\n\n")
      (suggest--insert-heading suggest--outputs-heading)
      (insert "\n3\n\n")
      (suggest--insert-heading suggest--results-heading)
      (insert "\n"))
    ;; Populate the suggestions for 1, 2 => 3
    (suggest-update)
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
    (loop-while (< headings-seen n)
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

(defun suggest--format-output (value)
  "Format VALUE as the output to a function."
  (let* ((lines (s-lines (suggest--pretty-format value)))
         (prefixed-lines
          (--map-indexed
           (if (zerop it-index) (concat ";=> " it) (concat ";   " it))
           lines)))
    (s-join "\n" prefixed-lines)))

;; TODO: why does SUGGESTION get *fontified* strings?
(defun suggest--format-suggestion (suggestion output)
  "Format SUGGESTION as a lisp expression returning OUTPUT."
  ;; SUGGESTION is a list that may contain strings, so we can show
  ;; e.g. #'foo rather than 'foo.
  (let* ((formatted-suggestion (format "%s" suggestion))
         ;; A string of spaces the same length as the suggestion.
         (matching-spaces (s-repeat (length formatted-suggestion) " "))
         (formatted-output (suggest--format-output output))
         ;; Append the output to the formatted suggestion. If the
         ;; output runs over multiple lines, indent appropriately.
         (formatted-lines
          (--map-indexed
           (if (zerop it-index)
               (format "%s %s" formatted-suggestion it)
             (format "%s %s" matching-spaces it))
           (s-lines formatted-output))))
    (s-join "\n" formatted-lines)))

(defun suggest--write-suggestions (suggestions output)
  "Write SUGGESTIONS to the current *suggest* buffer.
SUGGESTIONS is a list of forms."
  (->> suggestions
       (--map (suggest--format-suggestion it output))
       (s-join "\n")
       (suggest--write-suggestions-string)))

;; TODO: this is largely duplicated with refine.el and should be
;; factored out somewhere.
(defun suggest--pretty-format (value)
  "Return a pretty-printed version of VALUE."
  (let ((cl-formatted (with-temp-buffer
                        (cl-prettyprint value)
                        (s-trim (buffer-string)))))
    (cond ((stringp value)
           ;; TODO: we should format newlines as \n
           (format "\"%s\"" value))
          ;; Print nil and t as-is.'
          ((or (eq t value) (eq nil value))
           (format "%s" value))
          ;; Display other symbols, and lists, with a quote, so we
          ;; show usable syntax.
          ((or (symbolp value) (consp value))
           (format "'%s" cl-formatted))
          (t
           cl-formatted))))

(defun suggest--read-eval (form)
  "Read and eval FORM, but don't open a debugger on errors."
  (condition-case err
      (eval (read form))
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

(defun suggest--zip (&rest lists)
  "Zip LISTS together.  Group the head of each list, followed by the
second elements of each list, and so on. The lengths of the returned
groupings are equal to the length of the shortest input list.

Unlike dash 2.0, always uses lists."
  (let (results)
    (while (-none-p #'null lists)
      (setq results (cons (mapcar 'car lists) results))
      (setq lists (mapcar #'cdr lists)))
    (nreverse results)))

;; TODO: this would also be a good match for dash.el
(defun suggest--unzip (lst)
  "Inverse of `suggest--zip'.
Assumes all sublists are the same length."
  (let ((result nil))
    (dotimes (i (length (-first-item lst)) (nreverse result))
      (push (-select-column i lst) result))))

(defun suggest--possibilities (raw-inputs inputs output)
  "Return a list of possibilities for these INPUTS and OUTPUT.
Each possbility form uses RAW-INPUTS so we show variables rather
than their values."
  ;; E.g. ((1 "1") (2 "x"))
  (let* ((inputs-with-raws (suggest--zip inputs raw-inputs))
         ;; Each possible ordering of our inputs.
         (inputs-with-raws-perms (suggest--permutations inputs-with-raws))
         ;; E.g. (((1 2) ("1" "x")) ((2 1) ("x" "1")))
         (inputs-with-raws-perms-pairwise
          (-map #'suggest--unzip inputs-with-raws-perms))
         (possibilities nil))
    ;; Loop over every function.
    (loop-for-each func suggest-functions
      ;; For every possible input ordering,
      (loop-for-each inputs-raws-perm inputs-with-raws-perms-pairwise
        (-let [(inputs-perm raws-perm) inputs-raws-perm]
          ;; Try to evaluate the function.
          (ignore-errors
            (let ((func-output (apply func inputs-perm)))
              ;; If the function gave us the output we wanted:
              (when (equal func-output output)
                ;; Save the function with the raw inputs.
                (push (cons func raws-perm) possibilities)
                ;; Don't try any other input permutations for this
                ;; function.  This saves us returning multiple results
                ;; for functions that don't care about ordering, like
                ;; +.
                (loop-break))))))
      ;; If the input is a single list, try calling the function
      ;; variadically.
      (when (and
             (equal (length inputs) 1)
             (listp (-first-item inputs)))
        (ignore-errors
          (let ((func-output (apply func (-first-item inputs))))
            ;; If the function gave us the output we wanted:
            (when (equal func-output output)
              ;; Save the funcall form of calling this function.
              (push (list 'apply (format "#'%s" func) (-first-item raw-inputs))
                    possibilities))))))
    (nreverse possibilities)))

;;;###autoload
(defun suggest-update ()
  "Update the suggestions according to the latest inputs/output given."
  (interactive)
  ;; TODO: error on multiple inputs on one line.
  (let* ((raw-inputs (suggest--raw-inputs))
         (inputs (--map (suggest--read-eval it) raw-inputs))
         (raw-output (suggest--raw-output))
         (desired-output (suggest--read-eval raw-output))
         (possibilities
          (suggest--possibilities raw-inputs inputs desired-output)))
    (if possibilities
        (suggest--write-suggestions
         possibilities
         ;; We show the evalled output, not the raw input, so if
         ;; users use variables, we show the value of that variable.
         desired-output)
      (suggest--write-suggestions-string ";; No matches found.")))
  (suggest--update-needed nil)
  (set-buffer-modified-p nil))

(define-derived-mode suggest-mode emacs-lisp-mode "Suggest"
  "A major mode for finding functions that provide the output requested.")

(define-key suggest-mode-map (kbd "C-c C-c") #'suggest-update)

(defun suggest--update-needed (update-needed)
  "Update the suggestions heading to say whether we need
the user to call `suggest-update'."
  (save-excursion
    (goto-char (point-min))
    (suggest--nth-heading 3)
    (let ((inhibit-read-only t))
      (delete-region (point) (progn (move-end-of-line nil) (point)))
      (if update-needed
          (suggest--insert-heading
           (format ";; Suggestions (press %s to update):"
                   (key-description
                    (suggest--keybinding #'suggest-update suggest-mode-map))))
        (suggest--insert-heading suggest--results-heading)))))

(provide 'suggest)
;;; suggest.el ends here
