(
    ((+ 1 2 3) 6)
    ((+ (+ 8 13) 9 (+ 2 10)) 42)
    ((if #t (+ 1 2) (no_such_symbol)) 3)
    ((if #f (no_such_symbol) (+ 2 2)) 4)
    ((let ((x (+2 2))) (let ((x x)) x)) 4)
    ((= 3 4) #f)
    ((= 4 4) #t)
    ((null? (quote ())) #t)
    ((null? 3) #f)
    ((- (- 3 1) 1) 1)
    ((car (quote (2 3))) 2)
    ((cdr (quote (2 3))) (3))
    (((lambda (x y) (+ x y)) 1 2) 3)
    ((let ((f (lambda (x) (lambda (y) (+ x y))))) ((f 1) 2)) 3)
    ((letrec
        ((
            sum
            (lambda (x) (if (null? x) 0 (+ (car x) (sum (cdr x)))))
            ))
        (sum (quote (1 2 3 4 5))))
            15)
)