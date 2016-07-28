;;; suggest.el --- suggest elisp functions that give the output requested  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((loop "1.3") (dash "2.12.0") (s "1.11.0"))

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
(eval-when-compile
  (require 'cl-lib)) ;; cl-incf

;; TODO: support arbitrary orderings of arguments?
;; TODO: add (format %s _) somehow
(defvar suggest-functions
  '(identity
    ;; Build-in list functions.
    car
    cdr
    cadr
    cons
    nth
    list
    length
    reverse
    ;; CL list functions.
    first
    second
    third
    caddr
    ;; dash.el list functions.
    -non-nil
    -slice
    -take
    -take-last
    -drop
    -drop-last
    -select-by-indices
    -select-column
    -concat
    -flatten
    -replace
    -replace-first
    -insert-at
    -replace-at
    -remove-at
    -remove-at-indices
    -sum
    -product
    -min
    -max
    -is-prefix-p
    -is-suffix-p
    -is-infix-p
    -split-at
    -split-on
    -partition
    -partition-all
    -elem-index
    -elem-indices
    -union
    -difference
    -intersection
    -distinct
    -rotate
    -repeat
    -cons*
    -snoc
    -interpose
    -interleave
    -zip
    -first-item
    -last-item
    -butlast
    ;; Arithmetic
    +
    -
    *
    /
    %
    mod
    max
    min
    ash
    lsh
    log
    expt
    sqrt
    abs
    float
    round
    truncate
    ceiling
    fceiling
    ffloor
    fround
    ftruncate
    1+
    1-
    ;; Strings
    upcase
    downcase
    substring
    concat
    capitalize
    ;; s.el string functions
    s-trim
    s-trim-left
    s-trim-right
    s-chomp
    s-collapse-whitespace
    s-word-wrap
    s-left
    s-chop-suffix
    s-shared-start
    s-shared-end
    s-repeat
    s-concat
    s-prepend
    s-append
    s-lines
    s-split
    s-join
    s-ends-with-p
    s-starts-with-p
    s-contains-p
    s-replace
    s-capitalize
    s-index-of
    s-reverse
    s-count-matches
    s-split-words
    ;; Symbols
    symbol-name
    symbol-value
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

;; TODO: 'arguments' is probably clearer than 'inputs'.
(defvar suggest--inputs-heading ";; Inputs (one per line):")
(defvar suggest--outputs-heading ";; Desired output:")
(defvar suggest--results-heading ";; Suggestions:")

(defun suggest--insert-heading (text)
  "Highlight TEXT as a heading and insert in the current buffer."
  ;; Make a note of where the heading starts.
  (let ((start (point))
        end)
    ;; Insert the heading, ensuring it's not editable.
    (insert (propertize text 'read-only t))
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
    (forward-line 1)))

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

(defun suggest--write-suggestions (suggestions output)
  "Write SUGGESTIONS to the current *suggest* buffer.
SUGGESTIONS is a list of forms."
  (->> suggestions
       (--map (format "%s ;=> %s\n" it output))
       (s-join "\n")
       (suggest--write-suggestions-string)))

(defun suggest--pretty-format (value)
  "Return a pretty-printed version of VALUE."
  (cond
   ((null value) "nil")
   ((eq value t) "t")
   ((or (symbolp value) (consp value))
    (format "'%s" value))
   ((stringp value) (format "\"%s\"" value))
   (t (format "%s" value))))

(defun suggest--read-eval (form)
  "Read and eval FORM, but don't open a debugger on errors."
  (condition-case err
      (eval (read form))
    (error (user-error
            "Could not eval %s: %s" form err))))

(defun suggest-update ()
  "Update the suggestions according to the latest inputs/output given."
  (interactive)
  ;; TODO: error on multiple inputs on one line.
  (let* ((raw-inputs (suggest--raw-inputs))
         (inputs (--map (suggest--read-eval it) raw-inputs))
         (raw-output (suggest--raw-output))
         (desired-output (suggest--read-eval raw-output))
         (suggestions nil))
    (--each suggest-functions
      (ignore-errors
        (let ((func-output (apply it inputs)))
          (when (equal func-output desired-output)
            (push (-concat (list it) raw-inputs) suggestions)))))
    (if suggestions
        (suggest--write-suggestions
         (nreverse suggestions)
         ;; We show the evalled output, not the raw input, so if
         ;; users use variables, we show the value of that variable.
         (suggest--pretty-format desired-output))
      (suggest--write-suggestions-string ";; No matches found.")))
  (set-buffer-modified-p nil))

(define-derived-mode suggest-mode emacs-lisp-mode "Suggest"
  "A major mode for finding functions that provide the output requested.")

;; TODO: Pick one of these, both is silly
(define-key suggest-mode-map (kbd "<C-return>") #'suggest-update)
(define-key suggest-mode-map (kbd "C-c C-c") #'suggest-update)

(provide 'suggest)
;;; suggest.el ends here
