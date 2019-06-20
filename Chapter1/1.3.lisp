;Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(defun square (x)
  (* x x))

(defun theBigThree (a b c)
  (cond ((and (<= a b) (<= a c)) (+ (square b) (square c)))
        ((and (<= b a) (<= b c)) (+ (square a) (square c)))
        (t (+ (square a) (square b)))))
