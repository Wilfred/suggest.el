(require 'ert)
(require 'suggest)

(ert-deftest suggest--join-func-output ()
  (should
   (equal
    (suggest--join-func-output "(foo)" "1")
    "(foo) ;=> 1"))
  (should
   (equal
    (suggest--join-func-output "(foo)" "(1\n 2)")
    "(foo) ;=> (1\n      ;    2)")))

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
      (list call-fn call-fn-varadic)))
    ;; TODO: prefer functions whose arguments are in the order
    ;; specified by the user. This helps with 1 3 => 3.
    ))
