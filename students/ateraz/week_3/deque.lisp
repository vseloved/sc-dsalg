(defpackage :deque
  (:use :cl)
  (:export :deque-pop
           :deque-push
           :deque-shift
           :deque-unshift
           :empty-deque
           :linked-deque
           :stack-deque))

(in-package :deque)


(defclass abstract-deque ()
  ; Accessors are not used intentionally to make code
  ; more general with the minimal use of macros.
  ((head :initform nil)
   (tail :initform nil))
  (:documentation "Queue with insertions and extractions from both ends."))


(defmethod initialize-instance :after ((deque abstract-deque) &key values)
  (dolist (value values)
          (deque-push deque value)))


(defmethod deque-pop ((deque abstract-deque))
  (extract-from deque 'tail))


(defmethod deque-push ((deque abstract-deque) value)
  (insert-at deque 'tail value))


(defmethod deque-shift ((deque abstract-deque))
  (extract-from deque 'head))


(defmethod deque-unshift ((deque abstract-deque) value)
  (insert-at deque 'head value))


(define-condition empty-deque (error) ())


(defgeneric extract-from (deque end)
  (:documentation "Extract the value from one of the deque's end."))


(defgeneric insert-at (deque end val)
  (:documentation "Insert the value at one of the deque's ends.")
  (:method (deque end val)
           ; Insertion should not return value.
           nil))


(defun opposite (end)
  "Find opposite end of deque."
  (if (eq end 'head) 'tail 'head))
