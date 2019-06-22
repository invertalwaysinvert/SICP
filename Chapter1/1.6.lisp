;Exercise 1.6: Alyssa P. Hacker doesn’t see why if needs to be provided as a special form. “Why can’t I just define it as an ordinary procedure in terms of cond?” she asks. Alyssa’s friend Eva Lu Ator claims this can indeed be done, and she defines a new version of if:

(defun new-if (predicate then-clause else-clause)
  (cond (predicate then-clause)
        (t else-clause)))

(defun square (x)
  (* x x))

(defun average (x y)
  (/ (+ x y) 2))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun good-enough? (guess x)
  (< (abs (- (square guess) x)) 0.001))

; Alyssa uses it to rewrite the square-root program

(defun sqrt-iter (guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

(defun custom-sqrt (x)
  (sqrt 1.0 x))

; What happens when she attempts to use this to compute square roots?
;(custom-sqrt 4)
; Answer:
; What will happen is that we will be stuck in an infinite loop. The 'if' operator's expression is a special form in lisp where both the alternatives are not evaluated. Only the alternative that is needed based on the predicate gets evaluated. Here, in Alyssa's new-if all the operands will get evaluated so the recursion will never end.
