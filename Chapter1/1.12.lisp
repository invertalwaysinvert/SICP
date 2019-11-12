; Calculate the digits in Pascal's triangle

(defun pascal (level)
  "Returns all the numbers from level"
  (defun pp (ll n)
    (if (or (= ll 1) (= n 1) (= ll 2) (= n ll))
        1
        (+ (pp (- ll 1) n)
           (pp (- ll 1) (- n 1)))))

  (defun print-level (counter)
    (if (<= counter level)
        (print (pp level counter)))
    (if (<= counter level)
        (print-level (+ counter 1))))

  (print-level 1))

(pascal 2)
