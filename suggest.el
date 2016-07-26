;;; suggest.el --- suggest elisp functions that give the output requested  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((loop "1.3"))

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

;; TODO: support arbitrary orderings of arguments?
(defvar suggest-functions
  '(identity
    car
    cdr
    cadr
    +
    -
    *
    /)
  "Functions that suggest will consider.
These functions must not produce side effects.")

(defface suggest-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for headings."
  :group 'suggest)

(defvar suggest--inputs-heading "Inputs (one per line):")
(defvar suggest--outputs-heading "Desired output:")
(defvar suggest--results-heading "Suggestions:")

(defun suggest--heading (text)
  "Format TEXT as a heading."
  (propertize text
              'face 'suggest-heading
              'read-only t))

(defun suggest--raw-inputs ()
  "Read the input lines in the current suggestion buffer."
  (let ((raw-inputs nil))
    (loop-for-each-line
      ;; Skip over the inputs heading or lines without any content.
      (when (or (equal it suggest--inputs-heading)
                (equal it ""))
        (loop-continue))
      ;; Stop once we reach the outputs.
      (when (equal it suggest--outputs-heading)
        (loop-return (nreverse raw-inputs)))
      (push it raw-inputs))))

;; TODO: check that there's only one line of output, or prevent
;; multiple lines being entered.
(defun suggest--raw-output ()
  "Read the output line in the current suggestion buffer."
  (let ((seen-output-header nil))
    (loop-for-each-line
      ;; Skip empty lines.
      (when (equal it "")
        (loop-continue))
      ;; Note when we've seen the output header.
      (when (equal it suggest--outputs-heading)
        (setq seen-output-header t)
        (loop-continue))
      ;; The line after the output header is what we want.
      (when seen-output-header
        (loop-return it)))))

(defun suggest ()
  "Open a Suggest buffer that provides suggestions for the inputs
and outputs given."
  (interactive)
  (let ((buf (get-buffer-create "*suggest*")))
    (switch-to-buffer buf)
    (erase-buffer)
    (suggest-mode)
    (let ((inhibit-read-only t))
      (insert (suggest--heading suggest--inputs-heading)
              "\nnil\n\n"
              (suggest--heading suggest--outputs-heading)
              "\nnil\n\n"
              (suggest--heading suggest--results-heading)
              "\n(identity nil) ;=> nil"))))

(define-derived-mode suggest-mode fundamental-mode "Suggest"
  "A major mode for finding functions that provide the output requested.")

(provide 'suggest)
;;; suggest.el ends here
