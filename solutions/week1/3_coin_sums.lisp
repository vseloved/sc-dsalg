(defun coin-sums (coins target)
    "Compute number of different ways to make a target sum using given coins."
    (let ((ways (make-array (+ target 1))))
         (setf (aref ways 0) 1)
         (loop for coin in coins
            do (loop for sum from coin to target
                    do (setf (aref ways sum) (+ (aref ways sum) (aref ways (- sum coin))))))
         (aref ways target)))

(format T "~d~%"(coin-sums '(1 2 5 10 20 50 100 200) 200))

;;; Example output  time sbcl --script 3_coin_sums.lisp
;;; 73682
;;; sbcl --script 3_coin_sums.lisp  0,00s user 0,00s system 97% cpu 0,007 total
