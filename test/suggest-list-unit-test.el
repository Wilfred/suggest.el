(require 'ert)
(require 'suggest)

;; Unit tests of pure list functions.

(ert-deftest suggest-permutations-nil ()
  (should
   (equal (suggest--permutations nil)
          nil)))

(ert-deftest suggest-permutations-one-item ()
  (should
   (equal (suggest--permutations '(x))
          '((x)))))

(ert-deftest suggest-permutations-two-items ()
  (should
   (equal (suggest--permutations '(x y))
          '((x y) (y x)))))
