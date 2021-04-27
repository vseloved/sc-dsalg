(require "asdf")

(defvar +ascii-lowercase+
  (loop for i from (char-code #\a) to (char-code #\z) collect i)
  "All lowercase ASCII character codes.")


(defun decipher (file-name key-length)
  "Compute the sum of the ASCII values in decrypted plain English text."
  (loop
    with key-options = (loop repeat key-length collect +ascii-lowercase+)
    with generate-key = (cartesian-product key-options)
    for key = (funcall generate-key)
    while key
    with codes = (read-codes file-name)
    for values = (decode codes key key-length)
    for text = (coerce (mapcar #'code-char values) 'string)
    ; Assume that plain English text contains " the ".
    when (search " the " text)
    return (reduce '+ values)))


(defun read-codes (file-name)
  "Read ASCII codes from file."
  (with-open-file (stream file-name)
    (mapcar #'parse-integer
            (uiop:split-string (read-line stream) :separator ","))))


(defun cartesian-product (lists)
  "Create lazy cartesian product generator."
  (let ((head (car lists))
        ; Generator for nested products of (cdr lists) if they exist.
        (nested-gen nil))
    (labels (
             (generator ()
                        (unless head (return-from generator))
                        (unless (cdr lists)
                          (return-from generator (prog1
                                                  (list (car head))
                                                  (setq head (cdr head)))))
                        (unless nested-gen
                          (setq nested-gen (cartesian-product (cdr lists))))
                        (let ((nested (funcall nested-gen)))
                          (when nested
                            (return-from generator
                                         (nconc (list (car head)) nested))))
                        ; All nested products for current element are exhausted.
                        ; Move to the next element and reset nested products.
                        (setq head (cdr head))
                        (setq nested-gen nil)
                        (generator)))
            #'generator)))


(defun decode (codes key key-length)
  "Decodes ASCII values using key."
  (loop
    for i from 0
    for code in codes
    collect (logxor code (nth (mod i key-length) key))))


; Example output:
; $ time sbcl --script 4_cipher.lisp
; 129448
;
; real	0m0.392s
; user	0m0.364s
; sys 	0m0.028s
(write (decipher (merge-pathnames "4_cipher.txt" *load-truename*) 3))
