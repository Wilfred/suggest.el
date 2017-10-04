(require 'ert)
(require 'suggest)

(ert-deftest suggest-format-t ()
  (should
   (equal
    (suggest--pretty-format t) "t")))

(ert-deftest suggest-format-symbol ()
  (should
   (equal
    (suggest--pretty-format 'x) "'x")))

(ert-deftest suggest-format-string ()
  (should
   (equal
    (suggest--pretty-format "bar") "\"bar\"")))

(ert-deftest suggest-format-string-list ()
  (should
   (equal
    (suggest--pretty-format '("foo")) "'(\"foo\")")))

(ert-deftest suggest-format-output-symbol ()
  (should
   (equal
    (suggest--format-output 'foo) ";=> 'foo")))

(ert-deftest suggest-format-output-multiline ()
  (should
   (equal
    (suggest--format-output "foo\nbar") ";=> \"foo\n;   bar\"")))

(ert-deftest suggest-cmp ()
  (let ((add-constant '(:funcs ((:sym + :variadic-p nil))
                               :literals ("1" "1")))
        (call-fn '(:funcs ((:sym 1+ :variadic-p nil))
                          :literals ("1")))
        (call-fn-varadic '(:funcs ((:sym 1+ :variadic-p t))
                                  :literals ("1")))
        (call-long '(:funcs ((:sym very-obscure-function :variadic-p nil))
                            :literals ("1")))
        (call-2-fns '(:funcs ((:sym 1+ :variadic-p nil)
                              (:sym identity :variadic-p nil))
                             :literals ("1"))))
    ;; Prefer a single function call to two.
    (should
     (equal
      (-sort #'suggest--cmp-relevance (list call-fn call-2-fns))
      (list call-fn call-2-fns)))
    ;; Prefer shorter function names.
    (should
     (equal
      (-sort #'suggest--cmp-relevance (list call-fn call-long))
      (list call-fn call-long)))
    ;; Prefer fewer literals (we add literals when suggesting values).
    (should
     (equal
      (-sort #'suggest--cmp-relevance (list call-fn add-constant))
      (list call-fn add-constant)))
    ;; Prefer calling functions directly to `apply'.
    (should
     (equal
      (-sort #'suggest--cmp-relevance (list call-fn-varadic call-fn))
      (list call-fn call-fn-varadic)))))
