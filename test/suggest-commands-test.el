;;; suggest-command-test.el ---  -*- lexical-binding: t; -*-
(require 'ert)
(require 'suggest)

(ert-deftest suggest-smoke-test ()
  "Basic test to verify that the suggest command works."
  (suggest))

(ert-deftest suggest-existing-buffer ()
  "The suggest command should not error when called repeatedly."
  (suggest)
  (suggest))

(defmacro should-suggest (&rest inputs-output-form)
  "Given INPUT and OUTPUT, `suggest--possibilities' should propose FORM.
FORM must use _ for the user's input."
  (-let* (((inputs (output form)) (-split-on '=> inputs-output-form))
          (input-literals (-repeat (length inputs) "x")))
    `(let* ((possibilities
             (suggest--possibilities ',input-literals
                                     (list ,@inputs) ,output))
            (possibilities-funcs
             (--map (plist-get it :funcs) possibilities))
            (possibilities-funcs-syms
             (-map (lambda (funcs) (--map (plist-get it :sym) funcs))
                   possibilities-funcs))
            (expected-call
             ',(-butlast (-flatten form))))
       (should
        (member expected-call possibilities-funcs-syms)))))

(ert-deftest suggest-possibilities ()
  ;; Ensure we offer built-in list functions.
  (should-suggest '(a b c d) => '(c d)
                  (cdr (cdr _)))
  (should-suggest '(a b c d) => '(a b)
                  (butlast (butlast _)))
  ;; We should reorder arguments if it gets the result desired.
  (should-suggest 2 3 => 9
                  (expt _))
  ;; Compose functions.
  (should-suggest 2 3 => 7
                  (1- (expt _)))
  ;; Apply a list of arguments.
  (should-suggest '(2 3) => 5
                  (+ _))
  ;; This reuqires custom extra arguments for `format', specifically
  ;; "%o". TODO: make `should-suggest' assert this argument.
  (should-suggest '25 => "31"
                  (format _))
  ;; Regression test.
  (should-suggest '(a b c d) 'c => 2
                  (-elem-index _))
  (should-suggest 4 => '(1 2 3 4)
                  (number-sequence _)))

(ert-deftest suggest-higher-order-fns ()
  "We should not call higher order functions with symbols that
aren't whitelisted."
  ;; We should not end up calling the function `debug'.
  (suggest--possibilities
   (list "'debug")
   (list 'debug)
   'foo))
