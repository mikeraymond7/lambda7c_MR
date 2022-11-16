/*
(define square (lambda (n:int):int (* n n))) 
(let (z:float 4.5) (+ z z))
*/

(define x 1)
(setq x (+ x 1))
(display x)


// sum of i from 1 to 100:
(define sum 0)
(define i 1)
(while (<= i 100) (begin
  (setq sum (+ sum i))
  (setq i (+ i 1))
  ))
(display sum)
