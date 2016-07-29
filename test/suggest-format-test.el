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
