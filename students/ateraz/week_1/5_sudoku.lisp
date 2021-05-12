(defconstant +grid-size+ 9)
(defconstant +square-size+ 3
  "Size of single square on the grid.")
(defvar +valid-digits+ (loop for i from 1 to +grid-size+ collect i))


(defun solve-all-grids (file-name)
  "Solve all grids and compute sum of numbers in top left corners."
  (let ((next-grid (grid-generator file-name)))
    (loop
      for grid = (funcall next-grid)
      while grid
      sum (grid-key grid))))


(defun grid-generator (file-name)
  "Create generator that read sudoku puzzles from file."
  (let ((stream (open file-name))
        (grid (make-array (list +grid-size+ +grid-size+))))
    (lambda ()
            (cond
              ((not (open-stream-p stream)) nil)
              ((read-line stream nil) (read-grid stream grid))
              ((close stream) nil)
              (t nil)))))


(defun read-grid (stream grid)
  "Read single sudoku from opened file stream into grid."
  (loop
    for row below +grid-size+
    for line = (read-line stream)
    do (loop
         for col below +grid-size+
         for char across line
         do (setf (aref grid row col) (digit-char-p char)))
    finally (return grid)))


(defun grid-key (grid)
  "Solve grid puzzle and return number in top left corner."
  ; Check if puzzle already has number in top left corner.
  (when (loop for col below +square-size+ thereis (= 0 (aref grid 0 col)))
    ; Check if puzzle can be solved.
    (unless (solve-after-cell grid 0 0)
      (error "Can't solve puzzle ~A" grid)))
  (loop
    for col below +square-size+
    for digit = (aref grid 0 col)
    for magnitude = (expt 10 (- +square-size+ col 1))
    sum (* digit magnitude)))


(defun solve-after-cell (grid row col)
  "Solve sudoku puzzle represented by the grid in-place.
   All the cells before row and col are assumed to be already solved.
   Return T if grid is solved."
  ; Find first 0 after cell in (row, col) looking left-to-right top-to-bottom.
  (loop
    while (and (< row +grid-size+) (/= 0 (aref grid row col)))
    if (< col (1- +grid-size+))
    do (incf col)
    else do (incf row)
            (setq col 0))
  (when (= row +grid-size+)
    ; All cells are filled, puzzle is solved.
    (return-from solve-after-cell t))
  ; Try valid digits in place of 0.
  (loop
    for digit in +valid-digits+
    when (valid-digit-p digit grid row col)
    ; Set digit and try to solve recursively.
    do (setf (aref grid row col) digit)
       (when (solve-after-cell grid row col)
         (return-from solve-after-cell t)))
  ; Puzzle can not be solved. Previous cells were filled incorrectly.
  (setf (aref grid row col) 0)
  nil)


(defun valid-digit-p (digit grid row col)
  "Check if placing digit into grid in row and col does not violate rules."
  ; Check if digit is present in the cell's row or colum.
  (loop
    for i below +grid-size+
    when (or (= digit (aref grid i col)) (= digit (aref grid row i)))
    do (return-from valid-digit-p))
  ; Check if digit is present in the cell's square.
  (loop
    repeat +square-size+
    for i from (square-start row)
    do (loop
         repeat +square-size+
         for j from (square-start col)
         when (= digit (aref grid i j))
         do (return-from valid-digit-p)))
  t)


(defun square-start (coord)
  "Compute start of square by coordinate."
  (* +square-size+ (floor coord +square-size+)))


; Example output:
; $ time sbcl --script 5_sudoku.lisp
; 24702
;
; real	0m1.032s
; user	0m1.020s
; sys 	0m0.012s
(write (solve-all-grids (merge-pathnames "5_sudoku.txt" *load-truename*)))
