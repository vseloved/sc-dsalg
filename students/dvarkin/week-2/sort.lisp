(in-package :rtl-user)  ; need to (ql:quickload :rutils)


;;; SHELL SORT 

(defconstant +gaps+
  '(1750 701 301 132 57 23 10 4 1)
  "The best sequence of gaps, according to Marcin Ciura.")


(defun shell-insertion-sort (array step)
  (let ((len (length array)))
    (dotimes (i len)
      (do ((x (aref array i))
             (j i (- j step)))
            ((or (< (- j step) 0)
                 (< (aref array (1- j)) x))
	     (setf (aref array j) x))
          (setf (aref array j) (aref array (- j step)))))))


(defun shell-sort (array)
  ;;; Shell sort implementation
  (dolist (step +gaps+ array) ;; !!! modified original array at every step!!!
    (shell-insertion-sort array step)))

;;(rtl-user::print-sort-timings "shell-sort" 'rtl-user::shell-sort (rtl-user::random-vec 10000))




