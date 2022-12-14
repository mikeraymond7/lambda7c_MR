(define x:int (lambda (z:int y:int) (display y) (display "\n") (if (< z 1) y (x (- z 1) (+ y 1))))) (x 5 6)


// this works
(define q 5) (define x:int (lambda (z:int y:int)(define q 6)(+ (+ z y) q))) (x 1 2)

// this doesn't work // need to calculate closure
(define q 5) (define x:int (lambda (z:int y:int)(+ (+ z y) q))) (x 1 2)
