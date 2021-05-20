(in-package :deque-tests)

(def-suite unit-tests)

(in-suite unit-tests)

(defun run-unit-tests ()
  (run! 'unit-tests))

(test empty-deque
      :description "Test empty deques."
      (test-empty-deque 'deque:linked-deque)
      (test-empty-deque 'deque:stack-deque))

(test prepopulated-deque
      :description "Test deques with prepopulated values."
      (test-prepopulated-deque 'deque:linked-deque)
      (test-prepopulated-deque 'deque:stack-deque))


(defun test-empty-deque (deque-type)
  (let ((deque (make-instance deque-type)))
    (signals deque::empty-deque (deque:deque-pop deque))
    (signals deque::empty-deque (deque:deque-shift deque))
    (is (null (deque:deque-unshift deque 2)))
    (is (null (deque:deque-push deque 3)))
    (is (null (deque:deque-unshift deque 1)))
    (is (= 3 (deque:deque-pop deque)))
    (is (= 2 (deque:deque-pop deque)))
    (is (= 1 (deque:deque-shift deque)))
    (signals deque::empty-deque (deque:deque-shift deque))))


(defun test-prepopulated-deque (deque-type)
  (let ((deque (make-instance deque-type :values '(1 2 3))))
    (is (= 1 (deque:deque-shift deque)))
    (is (= 3 (deque:deque-pop deque)))
    (is (= 2 (deque:deque-pop deque)))
    (signals deque::empty-deque (deque:deque-pop deque))))
