(defconstant +Mon+ 0)
(defconstant +Sun+ 6)


(defun count-sundays (start end)
  "Count Sundays that fell on the first of the month between two dates."
  (let ((start-date (list-date start))
        (end-date (list-date end))
        (current-date (list-date '(1900 1 1) +Mon+)))
    (when (date< start-date current-date)
      (error "Only start dates after 1900-01-01 are supported."))
    ; Increment from current date to start date, maintaining day of week.
    (loop
      while (date< current-date start-date)
      do (next-month current-date))
    (loop
      while (date< current-date end-date)
      count (= (date-weekday current-date) +Sun+) into result
      do (next-month current-date)
      finally (return result))))


(defstruct date year month day (weekday +Mon+))


(defun list-date (date &optional (weekday +Mon+))
  "Convert list to date."
  (destructuring-bind
   (year month day) date
   (unless (<= 1 month 12)
     (error "Invalid date ~a - month should be 1 <= month <= 12" date))
   (let ((max-days (num-days-in-month year month)))
     (unless (<= 1 day max-days)
       (error "Invalid date ~a - day should be 1 <= day <= ~a" date max-days)))
   (make-date :year year :month month :day day :weekday weekday)))


(defun num-days-in-month (year month)
  "Return number of days in month depending on year and month."
  (cond
    ((member month '(4 6 9 11)) 30)
    ((/= month 2) 31)
    ((leap-year-p year) 29)
    (t 28)))


(defun leap-year-p (year)
  "Check is year is leap."
  (and (= 0 (mod year 4)) (or (= 0 (mod year 400)) (/= 0 (mod year 100)))))


(defun next-month (date)
  "Move date to the next month."
  (setf (date-weekday date)
        ; Compute how many days did not fit into complete week.
        (mod (+ (num-days-in-month (date-year date) (date-month date))
                (date-weekday date)) 7))
  (if (= 12 (date-month date))
    (progn
     (incf (date-year date))
     (setf (date-month date) 1))
    (incf (date-month date))))


(defun date< (first second)
  "Check if first date is before second."
  (loop
    for period in '(date-year date-month date-day)
    for first-value = (funcall period first)
    for second-value = (funcall period second)
    if (> first-value second-value) do (return nil)
    else if (< first-value second-value) do (return t)
    finally (return nil)))


; Example output:
; $ time sbcl --script 2_counting_sundays.lisp
; 171
;
; real	0m0.044s
; user	0m0.035s
; sys 	0m0.009s
(write (count-sundays '(1901 1 1) '(2000 12 31)))
