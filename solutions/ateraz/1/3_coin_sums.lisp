(defun coin-sums (coins target)
  "Compute number of ways to produce target amount using provided coins."
  ; Array where i-th element holds number of ways to produce amount i,
  ; requires one extra slot for zero sum.
  (let ((num-ways (make-array (+ target 1))))
    ; There's only one way to produce 0 pence.
    (setf (aref num-ways 0) 1)
    (loop
      for coin in coins
      do (loop
           for amount from coin to target
           ; Use one current coin to produce required amount for amount - coin.
           do (incf (aref num-ways amount) (aref num-ways (- amount coin)))))
    (aref num-ways target)))


; Example output:
; $ time sbcl --script 3_coin_sums.lisp
; 73682
;
; real	0m0.023s
; user	0m0.022s
; sys 	0m0.000s
(write (coin-sums '(1 2 5 10 20 50 100 200) 200))
