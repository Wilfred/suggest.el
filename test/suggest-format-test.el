(require 'ert)
(require 'suggest)

(ert-deftest suggest-format-t ()
  (should
   (equal
    (suggest--pretty-format t) "t")))
