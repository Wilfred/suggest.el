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

(ert-deftest suggest-unzip-nil ()
  (should (equal (suggest--unzip nil) nil)))

(ert-deftest suggest-unzip ()
  (should
   (equal (suggest--unzip '((x1 y1) (x2 y2) (x3 y3)))
          '((x1 x2 x3) (y1 y2 y3))))
  (should
   (equal (suggest--unzip '((x1 y1) (x2 y2) (x3 y3)))
          '((x1 x2 x3) (y1 y2 y3)))))
