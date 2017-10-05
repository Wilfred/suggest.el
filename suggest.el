;;; suggest.el --- suggest elisp functions that give the output requested  -*- lexical-binding: t; -*-

;; Copyright (C) 2017

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Version: 0.6
;; Keywords: convenience
;; Package-Requires: ((emacs "24.4") (loop "1.3") (dash "2.13.0") (s "1.11.0") (f "0.18.2"))
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

(require 'subr-x)

(defun foo (x)
  x)

(provide 'suggest)
;;; suggest.el ends here
