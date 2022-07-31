; create many object to test gc
(let ((loop 
    (letrec ((loop_rec 
        (lambda (i acc) (if (= i 0) acc (loop_rec (- i 1) (+ acc (car (cons 1 (quote ())))))))))
            (lambda (i) (loop_rec i 0)))))
        (loop 1000000))