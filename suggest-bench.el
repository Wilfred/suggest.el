;;; suggest-bench.el --- measure suggest.el performance

;;; Code:

(require 'suggest)
(require 'dash)
(require 'shut-up)

(defmacro suggest--print-time (form)
  "Evaluate FORM, and print the time taken."
  `(progn
     (message "Timing %s" ',form)
     (-let [(total-time gc-runs gc-time)
            (shut-up (benchmark-run 1 ,form))]
       (message "Elapsed time: %fs (%fs in %d GCs)"
                total-time
                gc-time
                gc-runs))))


(defun suggest-bench ()
  (interactive)
  (suggest--print-time
   (suggest--possibilities '("x" "y") '(1 2) 3))
  (suggest--print-time
   (suggest--possibilities '("x" "y") '(?a ?b ?c) ?c))
  (suggest--print-time
   (suggest--possibilities '("x") '((1 2 3 4 5)) 15)))

(provide 'suggest-bench)
;; suggest-bench.el ends here
