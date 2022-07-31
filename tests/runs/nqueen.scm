(define range 
    (letrec ((range_tail (lambda (start end acc)
        (if (= start end) acc (range_tail start (- end 1) (cons (- end 1) acc))))))
        (lambda (start end) (range_tail start end (quote ())))))
(define find_first (lambda (f l)
    (if 
        (null? l)
        #f
        (let ((r (f (car l))))
            (if r r (find_first f (cdr l)))))))
(define any
    (lambda (f l)
        (if (null? l)
            #f
            (if (f (car l)) #t (any f (cdr l))))))
(define nqueen
    (lambda (n)
        (letrec ((rec (lambda (cy acc)
            (if
                (= cy n)
                acc
                (find_first 
                    (lambda (cx)
                        (if 
                            (any
                                (lambda (pos) (let ((px (car pos)) (py (car (cdr pos))))
                                    (or (= px cx) (= (- py cy) (- px cx)) (= (- py cy) (- cx px)))))
                                acc)
                            #f
                            (rec (+ cy 1) (cons (cons cx (cons cy (quote ()))) acc))))
                    (range 0 n))))))
            (rec 0 (quote ())))))
(nqueen 8)