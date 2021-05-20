(in-package :deque)


(defclass stack-deque (abstract-deque) ()
  (:documentation "Deque implemented using two stacks."))


(defmethod extract-from ((deque stack-deque) end)
  (macrolet ((stack () '(slot-value deque end))
             (opposite-stack () '(slot-value deque (opposite end))))
            (unless (stack)
              (unless (opposite-stack)
                (error 'empty-deque))
              ; Move values from opposite stack to stack.
              (loop
                for value = (pop (opposite-stack))
                while value
                do (push value (stack))))
            (pop (stack))))


(defmethod insert-at :before ((deque stack-deque) end value)
  (push value (slot-value deque end)))
