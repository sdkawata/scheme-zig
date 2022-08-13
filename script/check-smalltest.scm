(define tests (call-with-input-file "./tests/small_tests.scm" read))
(map
    (lambda (pair)
        (let ((actual (eval (car pair) (interaction-environment))) (expected (cadr pair)))
            (if 
                (equal? expected actual)
                #t
                (begin
                    (display "expr: ")
                    (display (car pair))
                    (display " expected: ")
                    (display expected)
                    (display " actual: ")
                    (display actual)
                    (display "\n")
                    (raise "result mismatch")))))
    tests)