;Exercise 1.4.  Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; This procedure adds the absolute value of b to a
; (a-plus-abs-b 1 2) -> 3
; (a-plus-abs-b 1 -2) -> 3
; (a-plus-abs-b -1 2) -> 1
; (a-plus-abs-b -1 -2) -> 1
