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


(defun suggest-bench ()
  (interactive)
  ;; Basic arithmetic.
  (suggest--print-time
   (suggest--possibilities '("x" "y") '(1 2) 3))
  ;; List access.
  (suggest--print-time
   (suggest--possibilities '("x") '((?a ?b ?c)) ?c))
  ;; Zero results.
  (suggest--print-time
   (suggest--possibilities '("x") '("foo") "bar")))

(provide 'suggest-bench)
;; suggest-bench.el ends here
