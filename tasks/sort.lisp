(in-package :rtl-user)  ; need to (ql:quickload :rutils)


;; basic sorting algorithms

(defun selection-sort (array)
  (dotimes (i (1- (length array)))
    (let ((min (aref array i))
          (idx i))
      ;; find the minimum of the reminder of the array
      (dotimes (j (- (length array) i 1))
        (when (< (aref array (+ i j 1)) min)
          (setf min (aref array (+ i j 1))
                idx (+ i j 1))))
      ;; swap it in place
      (rotatef (aref array i) (aref array idx)))) 
  array)

(defun insertion-sort (array)
  (dotimes (i (1- (length array)))
    (dotimes (j (1+ i))
      (let ((j (- i j)))
        ;; while the next element is less then the previous, swap them
        (if (< (aref array (1+ j))
               (aref array j))
            (rotatef (aref array (1+ j))
                     (aref array j))
            (return)))))
  array)

(defun quicksort (array)
  (when (> (length array) 1)
    ;; simple pivot selection: just use the ast element
    (with ((pivot-i 0)
           (pivot (aref array (1- (length array)))))
          (dotimes (i (1- (length array)))
            (when (< (aref array i) pivot)
              (rotatef (aref array i)
                       (aref array pivot-i))
              (incf pivot-i)))
          ;; swap the pivot (last element) in its proper place
          (rotatef (aref array (1- (length array)))
                   (aref array pivot-i))
          (quicksort (slice array 0 pivot-i))
          (quicksort (slice array (1+ pivot-i)))))
  array)


;; test utilities

(defun random-vec (size)
  (let ((vec (make-array size)))
    (dotimes (i size)
      (setf (aref vec i) (random size)))
    vec))

(defun print-sort-timings (sort-name sort-fn vec)
  ;; we'll use in-place modification of the input vector VEC
  ;; so we need to copy it to preserve the original for future use
  (let ((vec (copy-seq vec))
        (len (length vec)))
    (format t "= ~Asort of random vector (length=~A) =~%"
            sort-name len)
    (time (funcall sort-fn vec))
    (format t "= ~Asort of sorted vector (length=~A) =~%"
            sort-name len)
    (time (funcall sort-fn vec))
    (format t "= ~Asort of reverse sorted vector (length=~A) =~%"
            sort-name len)
    (time (funcall sort-fn vec))))
