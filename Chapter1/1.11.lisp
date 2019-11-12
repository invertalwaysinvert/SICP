; Exercise 1.11

(defun fn (n)
  (if (< n 3)
      n
      (+ (fn (- n 1))
         (* 2 (fn (- n 2)))
         (* 3 (fn (- n 3))))))


(defun fn-iter (n)
  (defun ff (current counter a b)
    (if (> counter n)
        current
        (ff (+ current (* 2 a) (* 3 b))
            (+ counter 1)
            current
            a)))
  (if (< n 3)
      n
      (ff 4 4 2 1)))


(print (fn 34))
(print (fn-iter 34))
