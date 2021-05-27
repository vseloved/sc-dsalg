(defpackage :hashtable
  (:use :cl)
  (:export :htinit
           :htset
           :htget
           :htrem
           :htmap))

(in-package :hashtable)


(defstruct (node (:conc-name nil))
  "Node in a linked list that is used to represent insertion order."
  key val prev next)


(defstruct (ht (:conc-name nil))
  "Order-preserving hash table, uses chaining for collision resolution."
  ; Vector of chains of node whose keys produce the same hash.
  chains
  ; Number of elements currently stored in hash table.
  size
  ; Doubly linked list that represents order of elements in hashtable.
  head tail
  ; Default value that is returned when looking up missing key.
  default)


(defparameter *usable-fraction* .8
  "Hash table size threshold, exceeding it causes doubling table capacity.")


(defmacro get-chain (ht key)
  `(aref (chains ,ht) (get-chain-index (chains ,ht) ,key)))


(defun htinit (&key (default nil) (capacity 2))
  "Create hash table with required capacity."
  (make-ht :chains (create-chains capacity) :size 0 :default default))


(defun htset (ht key val)
  "Add key to hash table. Remove previous key and resize hash table if needed."
  (let ((capacity (length (chains ht))))
    (when (> (size ht) (* capacity *usable-fraction*))
      ; Double hast table capacity and copy previous values there.
      (let ((new-chains (create-chains (* 2 capacity))))
        (loop
          for chain across (chains ht)
          do (loop
               for node in chain
               do (push node (aref new-chains
                                   (get-chain-index new-chains (key node))))))
        (setf (chains ht) new-chains))))
  (htrem ht key)
  (let ((node (make-node :key key :val val)))
    (if (tail ht)
      ; Connect tail to node.
      (setf (next (tail ht)) node)
      ; Order list is empty, make node a head of order list.
      (setf (head ht) node))
    ; Connect node to tail and make it a new tail.
    (setf (prev node) (tail ht))
    (setf (tail ht) node)
    ; Add node to collision resolution chain.
    (push node (get-chain ht key)))
  (incf (size ht)))


(defun htget (ht key)
  "Get value from hash table by key. Return NIL if key is not present."
  (let ((node (htnode ht key)))
    (if node
      (val node)
      (default ht))))


(defun htrem (ht key)
  "Remove key from hash table if it is present."
  (let ((node (htnode ht key)))
    (unless node (return-from htrem))
    ; Remove node from collision resolution chain.
    (let ((key (key node)))
      (setf (get-chain ht key)
            (delete-if #'(lambda (node) (has-key node key))
                       (get-chain ht key))))
    ; Remove node from linked list representing order.
    (let ((prev-node (prev node))
          (next-node (next node)))
      (when prev-node
        (setf (next prev-node) next-node))
      (when next-node
        (setf (prev next-node) prev-node))
      (when (eql node (head ht))
        (setf (head ht) next-node))
      (when (eql node (tail ht))
        (setf (tail ht) prev-node)))
    (decf (size ht))))


(defun htmap (fn ht)
  "Map hash table in order of insertion."
  (loop
    for node = (head ht) then (next node)
    while node
    collect (funcall fn (key node) (val node))))


; Helpers


(defun htnode (ht key)
  "Get node from hash table by key. Return nil if key is not present."
  (loop
    for node in (get-chain ht key)
    when (has-key node key)
    do (return node)))


(defun create-chains (capacity)
  (make-array capacity :initial-element nil))


(defun has-key (node key)
  (equal key (key node)))


(defun get-chain-index (chains key)
  "Get index of key in a vector of collision resolution chains."
  (mod (sxhash key) (length chains)))
