(display
    (let
            (
                (sum
                    (letrec ((sum_tail (lambda (x acc) (if (null? x) acc (sum_tail (cdr x) (+ acc (car x)))))))
                        (lambda (x) (sum_tail x 0))))
                (range
                    (letrec ((range_tail (lambda (start end acc)
                        (if (= start end) acc (range_tail start (- end 1) (cons (- end 1) acc))))))
                        (lambda (start end) (range_tail start end (quote ()))))))
            (sum (range 1 1000))))