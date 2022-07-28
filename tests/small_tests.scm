(
    ((+ 1 2 3) 6)
    ((+ (+ 8 13) 9 (+ 2 10)) 42)
    ((if #t (+ 1 2) (no_such_symbol)) 3)
    ((if #f (no_such_symbol) (+ 2 2)) 4)
)