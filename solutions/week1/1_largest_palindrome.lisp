(defun largest-palindrome (n)
    "Calculates the largest palindrom of 2 n-digit numbers product."
    (when (eq n 1) (return 9))

    (let* ((hi (expt 10 n))
           (max_a (- hi (expt 10 (- n 1)))))
    
          (loop named zz for a from 2 to max_a
                do (let* ((left (- hi a))
                           (right (parse-integer (reverse (write-to-string left))))
                           (D (- (expt a 2) (* 4 right)))) 
                          (when (>= D 0)
                            (let* ((root1 (/ (+ a (sqrt D)) 2))
                                   (root2 (/ (- a (sqrt D)) 2))))
                                (if (some 'integerp '(root1 root2))
                                    (return (+ (* hi left) right)))))))
           nil)
