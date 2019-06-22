;Exercise 1.8: Newton’s method for cube roots is based on the fact that if y is an approximation to the cube root of x, then a better approximation is given by the value

(defun cube-root (x)
  (cube-root-iterator 1.0 x))

(defun cube-root-iterator (guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iterator (improve guess x) x)))

(defun good-enough? (guess x)
  (< (abs (- (cube guess) (cube (improve guess x)))) 0.0001))

(defun cube (x)
  (* x x x))

(defun improve (guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(print (cube-root 64.0))
