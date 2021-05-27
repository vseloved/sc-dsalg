(defpackage :hashtable-tests
  (:use :cl :fiveam :hashtable)
  (:export :run-unit-tests))

(in-package :hashtable-tests)

(def-suite unit-tests)

(in-suite unit-tests)

(defun run-unit-tests ()
  (run! 'unit-tests))

(test default-hashtable-value
      (let ((ht (htinit :default 99)))
        (is (= 99 (htget ht "key")))))

(test add-to-hashtable
      (let ((ht (htinit)))
        (htset ht "key1" 1)
        (is (= 1 (htget ht "key1")))
        (htset ht "key2" 2)
        (is (= 2 (htget ht "key2")))))

(test resize-hashtable
      (let ((ht (htinit)))
        (htset ht "key1" 3)
        (htset ht "key2" 2)
        (htset ht "key3" 1)
        (is (= 3 (htget ht "key1")))
        (is (= 2 (htget ht "key2")))
        (is (= 1 (htget ht "key3")))))

(test remove-from-hashtable
      (let ((ht (htinit)))
        (htset ht "key" 1)
        (htrem ht "key")
        (is (null (htget ht "key")))))

(test override-key
      (let ((ht (htinit)))
        (htset ht "key" 1)
        (htset ht "key" 2)
        (is (= 2 (htget ht "key")))
        (htrem ht "key")
        (is (null (htget ht "key")))))

(test map-hashtable
      (let ((ht (htinit)))
        (loop
          for i from 0 to 5
          do (htset ht (format nil "key~a" i) i))
        ; Remove and override exising keys to check the resulting order.
        (htrem ht "key0")
        (htset ht "key4" 9)
        (loop
          for x in (htmap #'(lambda (k v) (declare (ignore k)) (1+ v)) ht)
          for y in '(2 3 4 6 10)
          do (is (= x y)))))
