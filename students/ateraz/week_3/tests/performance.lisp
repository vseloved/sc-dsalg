(in-package :deque-tests)

(defvar +extractions+ '(deque:deque-pop deque:deque-shift)
  "Methods that extract from the deque.")
(defvar +insertions+ '(deque:deque-push deque:deque-unshift)
  "Methods that insert into the deque.")
(defconstant +max-arg+ 100
  "Max value to insert into the deque.")
(defvar +methods+ (append +extractions+ +insertions+))
(defvar +num-methods+ (length +methods+))


(defun generate-method-call ()
  (let ((method (nth (random +num-methods+) +methods+)))
    (if (member method +extractions+)
      ; Extraction may cause error when called on empty deque.
      (lambda (deque) (handler-case (funcall method deque)
                                    (deque::empty-deque () nil)))
      ; Insertion requires value argument.
      ; Value is generated outside of lambda to make all calls identical.
      (let ((value (random +max-arg+)))
        (lambda (deque) (funcall method deque value))))))


(defun test-deque-performance (deque-type method-calls)
  (let ((deque (make-instance deque-type)))
    (format t "~A:~%" deque-type)
    (time (dolist (method-call method-calls)
                  (funcall method-call deque)))))


(defun run-performance-tests (&optional (num-calls (expt 10 6)))
  (let ((method-calls (loop repeat num-calls collect (generate-method-call))))
    (format t "Running performance tests~%")
    (test-deque-performance 'deque:linked-deque method-calls)
    (test-deque-performance 'deque:stack-deque method-calls)))
