(defsystem :deque
  :components ((:file "deque")
               (:file "linked-deque")
               (:file "stack-deque"))
  :in-order-to ((test-op (test-op :deque/tests))))

(defsystem :deque/tests
  :depends-on (:deque :fiveam)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "performance")
                             (:file "unit"))))
  :perform (test-op (o s)
              (symbol-call 'deque-tests 'run-unit-tests)
              (symbol-call 'deque-tests 'run-performance-tests)))
