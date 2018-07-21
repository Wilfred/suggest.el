;;; suggest-bench.el --- measure suggest.el performance

;;; Code:

(require 'suggest)
(require 'dash)
(require 'shut-up)

(defmacro suggest--print-time (form)
  "Evaluate FORM, and print the time taken."
  `(progn
     (message "%s" ',form)
     (-let [(total-time gc-runs gc-time)
            (shut-up (benchmark-run 1 ,form))]
       (message "  time: %fs (%fs in %d GCs)"
                total-time
                gc-time
                gc-runs))))


(defun suggest--possibilities-bench ()
  (interactive)
  ;; Basic arithmetic.
  (suggest--print-time
   (suggest--possibilities '("x" "y") '(1 2) 3))
  ;; List access.
  (suggest--print-time
   (suggest--possibilities '("x") '((?a ?b ?c)) ?c))
  ;; List generation
  (suggest--print-time
   (suggest--possibilities '("x") '(4) '(1 2 3 4)))
  ;; Zero results.
  (suggest--print-time
   (suggest--possibilities '("x") '("foo") "bar")))

(defun suggest-bench ()
  (interactive)
  ;; Overall call time.
  (suggest--print-time
   (suggest)))

(provide 'suggest-bench)
;; suggest-bench.el ends here
