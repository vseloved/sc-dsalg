(defun largest-palindrome-product (num-digits)
  "Compute largest palindrom made from product of two num-digits numbers."
  (let* ((min-value (expt 10 (1- num-digits)))
         (max-value (1- (* 10 min-value)))
         (start max-value)
         (step 1))

    (when (> num-digits 1)
      ; Optinal optimization that assumes that resulting palindrome has
      ; 2 * num-digits, thus it is divisible by 11. This allows to start
      ; with first number lower or equal than max-value such that
      ; start % 11 == 0 and skip all i that are not divisible by 11.
      (setq step 11)
      (loop
        while (/= 0 (mod start step))
        do (decf start)))

    (loop
      with max-product = min-value and max-j = min-value
      for i from start downto min-value by step
      do (loop
           named inner
           ; As i is always decreasing, there is no point to iterate
           ; further than previous j that participated in max-product.
           for j from max-value downto max-j by 1
           for product = (* i j)
           while (> product max-product)
           when (palindrome-p product)
           do (setq max-product product)
              (setq max-j j)
              (return-from inner))
      finally (return max-product))))


(defun palindrome-p (num)
  "Check if number is palindrome."
  (let ((s (write-to-string num)))
    (equal s (reverse s))))


; Example output:
; $ time sbcl --script 1_largest_palindrome_product.lisp
; Largest 1-digits palindrome product 9
; Largest 2-digits palindrome product 9009
; Largest 3-digits palindrome product 906609
; Largest 4-digits palindrome product 99000099
; Largest 5-digits palindrome product 9966006699
; Largest 6-digits palindrome product 999000000999
;
; real	0m0.082s
; user	0m0.078s
; sys 	0m0.004s
(loop for i from 1 to 6
  do (format t "Largest ~d-digits palindrome product ~d~%"
             i (largest-palindrome-product i)))
