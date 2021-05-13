(require "bt-semaphore")  ; need to (ql:quickload "bt-semaphore")
; Proposed sorting candidates:

(defun lazy-parallel-quick-sort (arr)
  "Lazy sorting using parallel quick sort."
  (lazy-sort arr 'parallel-quick-sort))


(defun cheat-sort (arr)
  "Lazy sorting using radix sort."
  (lazy-sort arr 'radix-sort))


; Helper functions:

(defun lazy-sort (arr sort)
  "Sorting that check if array is sorted or reverse sorted before sorting it."
  (when (sorted-p arr #'<)
    (return-from lazy-sort arr))
  (when (sorted-p arr #'>)
    (return-from lazy-sort (reverse-sorted arr)))
  (funcall sort arr))


(defun sorted-p (arr comparator)
  "Check if array is sorted."
  (let ((size (length arr)))
    (when (< size 2)
      (return-from sorted-p t))
    (let ((current)
          (prev (aref arr 0)))
      (loop
        for i from 1 to (1- size)
        do (setf current (aref arr i))
        when (funcall comparator current prev)
        do (return-from sorted-p)
        else do (setf prev current)
        finally (return-from sorted-p t)))))


(defun reverse-sorted (arr)
  "Reverse array that is sorted in non-increasing order."
  (loop
    with size = (length arr)
    for left below size
    for right downfrom (1- size)
    while (< left right)
    do (rotatef (aref arr left) (aref arr right)))
  arr)


(defconstant +thread-split-threshold+ 1000
  "Use separate threads to sort array that are bigger threshold.")


(defun quicksort (arr start stop)
  "Quicksort part of array from start to stop. Uses threads for large parts."
  (when (< start stop)
    (let ((pivot-i start)
          (pivot))
      ; Select random pivot and put it at the end of array.
      (rotatef (aref arr stop) (aref arr (+ start (random (- stop start)))))
      (setf pivot (aref arr stop))
      (loop
        for i from start upto stop
        when (< (aref arr i) pivot)
        do (rotatef (aref arr i) (aref arr pivot-i))
           (incf pivot-i))
      (rotatef (aref arr stop) (aref arr pivot-i))
      (if (> (- stop start) +thread-split-threshold+)
        ; Only use threads for large parts.
        (let ((left-thread (sb-thread:make-thread
                            (lambda () (quicksort arr start (1- pivot-i)))))
              (right-thread (sb-thread:make-thread
                             (lambda () (quicksort arr (1+ pivot-i) stop)))))
          (bt:join-thread left-thread)
          (bt:join-thread right-thread))
        (progn
         (quicksort arr start (1- pivot-i))
         (quicksort arr (1+ pivot-i) stop))))))


(defun parallel-quick-sort (arr)
  "Parallel quick sort using threads."
  (quicksort arr 0 (1- (length arr)))
  arr)


(defun radix-sort (arr)
  "Minimal version of radix sort for positive integers using lists."
  (let ((l (coerce arr 'list)))
    (dotimes (bit (integer-length (reduce #'max l)))
             (let ((zeros) (ones))
               (dolist (x l)
                       (if (logbitp bit x)
                         (push x ones)
                         (push x zeros)))
               (setq l (nconc (nreverse zeros) (nreverse ones)))))
    (coerce l 'vector)))
