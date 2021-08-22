#lang sicp

"Ex 1.1"

10                     ; 10
(+ 5 3 4)              ; 12
(- 9 1)                ; 8
(/ 6 2)                ; 3 
(+ (* 2 4) (- 4 6))    ; 6
(define a 3)
(define b (+ a 1))
(+ a b (* a b))        ; 19
(= a b)                ; false?
(if (and (> b a) (< b (* a b)))
    b
    a)                 ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))       ; 16
(+ 2 (if (> b a) b a)) ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))    ; 16
   (+ a 1))



"Ex 1.2"

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))




"Ex 1.3"

(define (square x) (* x x))
(define (sum-of-square x y) (+ (square x) (square y)))
(define (func13 x y z)
  (cond ((>= x y) (if (> y z) (sum-of-square x y) (sum-of-square x z)))
        (else (if (> x z) (sum-of-square x y) (sum-of-square y z)))))



"Ex 1.4"

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b)) ; The following evaluates whether (+ a b) or (- a b) occurs
                          ; (- a b) will yield the correct value because you will be subtracting by *b* which is a negative integer

