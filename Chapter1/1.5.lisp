;Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures:

(defvar `(p) `(p))

(defun test (x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

; Applicative-order
; It would be stuck in a loop as it would keep on evaluating (p) -> (p) -> (p) -> ...

; Normal-order
0

