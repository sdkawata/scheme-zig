(
    (1 1)
    (#\a #\a)
    ((+ 1 2 3) 6)
    ((* 1 2 3) 6)
    ((+ 1 .5) 1.5)
    ((* 2 0.5) 1.0)
    ((+ (+ 8 13) 9 (+ 2 10)) 42)
    ((- 10 1) 9)
    ((- 1 10) -9)
    ((- 10) -10)
    ((- 10 1.) 9.)
    ((/ 1 2.) .5)
    ((> 2 1) #t)
    ((> 2. 1.) #t)
    ((> 1. 2.) #f)
    ((if #t (+ 1 2) (no_such_symbol)) 3)
    ((if #f (no_such_symbol) (+ 2 2)) 4)
    ((+ 3 (if #t 3 4)) 6) ; non-tail-position if
    ((let ((x (+ 2 2))) (let ((x x)) x)) 4)
    ((+ 3 (let ((x 1)) (+ 1 x))) 5) ;non-tail-poision let
    ((= 3 4) #f)
    ((= 4 4) #t)
    ((and) #t)
    ((and (= 1 2) (no_such_symbol)) #f)
    ((and #t #t (+ 1 1)) 2)
    ((or) #f)
    ((or (= 1 1) (no_such_symbol)) #t)
    ((or #f #f (+ 1 1)) 2)
    ((+ (or 1) 1) 2) ; non-tail-position or
    ((null? (quote ())) #t)
    ((null? 3) #f)
    ((- (- 3 1) 1) 1)
    ((car (quote (2 3))) 2)
    ((cdr (quote (2 3))) (3))
    ((cons 1 (quote (3))) (1 3))
    (((lambda (x y) (+ x y)) 1 2) 3)
    ((let ((x 1)) (set! x 2) x) 2)
    ((cond ((= 0 0) 1)) 1)
    ((cond ((= 0 1) (no_such_symbol)) (else 2)) 2)
    ((null? (cond (#t (quote ())))) #t) ; non-tail-position cond
    ((null? (cond (#f (quote ())) (else (quote (1))))) #f); non-tail-posiiton cond with else
    ((let ((f (lambda (x) (lambda (y) (+ x y))))) ((f 1) 2)) 3) ; closure
    ((+ 3 (letrec ((x 1)) (+ 1 x))) 5) ;non-tail-poision letrec
    ((begin 1 2) 2)
    ((letrec
        ((
            sum
            (lambda (x) (if (null? x) 0 (+ (car x) (sum (cdr x)))))
            ))
        (sum (quote (1 2 3))))
            6)
)