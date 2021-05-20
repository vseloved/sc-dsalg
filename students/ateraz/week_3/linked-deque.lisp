(in-package :deque)


(defclass linked-deque (abstract-deque) ()
  (:documentation "Deque implemented using doubly linked list."))


(defstruct node
  value prev next)


(defmacro with-connections ((end from-node from-end) &body body)
  "Determine connection fields in nodes by deque's end."
  `(multiple-value-bind (,from-node ,from-end)
                        (if (eq ,end 'head)
                          ; New node connects to head via next field.
                          ; Head connects to new node via prev field.
                          (values 'next 'prev)
                          ; New node connects to tail via prev field.
                          ; Tail connects to new node via next field.
                          (values 'prev 'next))
                        ,@body))


(defmethod extract-from ((deque linked-deque) end)
  (with-connections (end from-node from-end)
    (macrolet ((end-node () '(slot-value deque end)))
              (let ((extracted-node (end-node)))
                (unless extracted-node
                  (error 'empty-deque))
                ; Mark previous node counting from end as new end.
                (setf (end-node) (slot-value extracted-node from-node))
                (if (end-node)
                  ; Remove connection from new end to extacted node.
                  (setf (slot-value (end-node) from-end) nil)
                  ; No nodes remains in deque, mark opposite end as nil.
                  (setf (slot-value deque (opposite end)) nil))
                (node-value extracted-node)))))


(defmethod insert-at :before ((deque linked-deque) end value)
  (with-connections (end from-node from-end)
    (macrolet ((end-node () '(slot-value deque end)))
              (let ((new-node (make-node :value value)))
                ; Connect new node to end node.
                (setf (slot-value new-node from-node) (end-node))
                (if (end-node)
                  ; Connect end node to new node.
                  (setf (slot-value (end-node) from-end) new-node)
                  ; Only one node remains in deque, mark it as opposite end.
                  (setf (slot-value deque (opposite end)) new-node))
                ; Make new node the end.
                (setf (end-node) new-node)))))
