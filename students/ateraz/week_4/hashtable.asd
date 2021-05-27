(defsystem :hashtable
  :components ((:file "hashtable"))
  :in-order-to ((test-op (test-op :hashtable/tests))))

(defsystem :hashtable/tests
  :depends-on (:hashtable :fiveam)
  :components ((:file "tests"))
  :perform (test-op (o s) (symbol-call 'hashtable-tests 'run-unit-tests)))
