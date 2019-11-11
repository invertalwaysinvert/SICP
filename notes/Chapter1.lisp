; Basics of LISP
; It is important to understand the basic ideas and content behind the design of a language and to understand it's basic building blocks

; As I am working my way through the book using Common Lisp, I will need to learn the syntax of common lisp and it's special forms as opposed to Scheme that is being used in the book

; For defining a variable
(defvar a 5)

; Assigning another value to a predefined variable
(setf a 4)

; Defining a procedure
(defun square (x)
  "Returns the square a number"
  (* x x))

; Think of a lisp expression as a tree. You start the evaluation by working your way up from the edges of the tree. You find the value of each point where the tree has branched out and then work your way upwards towards the root node.

; Substitution model is one of the way to understand how a Lisp procedure is being evaluated but it's not how the interpreter actually works and the model breaks down at a point when we start dealing with mutable data.

; The "fully expand and then reduce" evaluation method is known as normal-order evaluation, in contrast to the "evaluate the arguments and then apply" method that the interpreter actually uses, which is called applicative-order evaluation.

; We can use something resembling a switch syntax in lisp
(defun absolute (x)
  "Returns the absolute value of a number"
  (cond ((> x 0) x)
        ((< x 0) (- x))
        (t 0)))

(defun absolute-alter (x)
  (cond ((< x 0) (-x))
        (t x)))

(defun absolute-if (x)
  (if (< x 0)
      (- x)
      x))

(and nil nil)
; nil
(and nil t)
; nil
(or t t)
; t
(or t nil)
; t
(not t)
; nil
(not nil)
; t

; Procedure to find the Square Root of a number
(defun average (x y)
  (/ (+ x y) 2))

(defun improve-guess (x guess)
  "Procedure to improve the guess"
  (average guess (/ x guess)))

(defun good-enough (x guess)
  "Procedure to see if a guess is good enough"
  (<= (abs (- (square guess) x)) 0.0001))

(defun sqrt-alter (x guess)
  "Procedure to find the square root of a number using Newton's method"
  (if (good-enough x guess)
      guess
      (sqrt-alter x (improve-guess x guess))))

(print (float (sqrt-alter 16 1)))

; Procedure to find the Cube root of a number
(defun cube-root (x guess)
  "Procedure returns the cube root of a number"
  (if (good-enough-cube x guess)
      guess
      (cube-root x (improve-cube-guess x guess))))

(defun good-enough-cube (x guess)
  "Procedure to check if the guess is good enough"
  (< (abs (- x (cube guess))) 0.00001))

(defun improve-cube-guess (x guess)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defun cube (x)
  (* x x x))

(print (float (cube-root 29 1)))

; Block Structure to keep the name space clean and Lexical Scoping to simplify things
(defun sqrt-hidden (x guess)
    (defun improve-guess-hidden (guess)
      "Procedure to improve the guess"
      (average-hidden guess (/ x guess)))

    (defun good-enough-hidden (guess)
      "Procedure to see if a guess is good enough"
      (<= (abs (- (square guess) x)) 0.0001))

    (defun average-hidden (x y)
      (/ (+ x y) 2))

    "Procedure to find the square root of a number using Newton's method"
    (if (good-enough-hidden guess)
        guess
        (sqrt-hidden x (improve-guess-hidden guess))))

(print (float (sqrt-hidden 4 1)))

; Factorial
(defun factorial (n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(defun factorial (n)
  (fact-iter 1 1 n))

(defun fact-iter (product counter max-count)
  (if (= counter max-count)
      (product)
      (fact-iter (* product counter)
                 (+ counter 1)
                 max-count)))
