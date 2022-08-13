(define (fractional n) (if (= n 1) 1 (* n (fractional (- n 1)))))
(display (fractional 5))