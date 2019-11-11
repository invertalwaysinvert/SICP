;Exercise 1.3.  Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(defun square (x)
  (* x x))

(defun theBigThree (a b c)
  (cond ((and (<= a b) (<= a c)) (+ (square b) (square c)))
        ((and (<= b a) (<= b c)) (+ (square a) (square c)))
        (t (+ (square a) (square b)))))

(defun sumOfLargest (a b c)
  (if (>= a b)
      (if (>= c b)
          (+ (square a) (square c))
          (+ (square a) (square b)))
      (if (>= c a)
          (+ (square b) (square c))
          (+ (square a) (square b)))))

(print (theBigThree 1 2 3))
(print (sumOfLargest 1 2 3))
(print (theBigThree 5 2 3))
(print (sumOfLargest 5 2 3))
(print (theBigThree 1 3 3))
(print (sumOfLargest 1 3 3))
(print (theBigThree 7 4 3))
(print (sumOfLargest 7 4 3))

