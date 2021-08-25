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

"Ex 1.5"
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))

; Applicative order begins by replacing the value of (p) in (test 0 (p)) with (p)
; Since (p) is again just itself, we must replace (p) with (p) once again
; This process is endless and we are stuck in an infinite loop

; Normal order just accepts the value of (test 0 (p)) and proceeds with the replacement of the procedure name (test)
; (if (= 0 0)
;     0
;     (p))

; (if #t
;     0
;     (p))

; 0

; As seen above, the procedure (p) is never evaluated and thus we are not in an infinite loop

;;;;;;;


; Guess    Quotient              Average
; 1        (2/1) = 2             ((2 + 1)/2) = 1.5
; 1.5      (2/1.5) = 1.3333      ((1.3333 + 1.5)/2) = 1.4167
; 1.4167   (2/1.4167) = 1.4118   ((1.4167 + 1.4118)/2) = 1.4142
; 1.4142   ...                   ...


(define (average x y)
  (/ (+ x y) 2))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 2)
(sqrt 3)

"Ex 1.6"

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new-if (improve guess x)
                            x)))

(define (sqrt-new-if x)
  (sqrt-iter-new-if 1.0 x))

; After testing this, the error is "maximum recursion depth exceeded"
; Scheme is an *applicative-order* language
; Macros, such as ifs and conds go through normal-order

; (sqrt 2)

; (sqrt-iter 1.0 2)

; (if (good-enough? 1.0 2)
;     1.0
;     (sqrt-iter (improve 1.0 2)
;                2))

; The predicate is first evaluated, THEN the arguments are evaluated
; (if #f
;     1.0
;     (sqrt-iter (improve 1.0 2)
;                2))

; (sqrt-iter (improve 1.0 2)
;            2))

; (sqrt-iter 1.5 2))

; and so on.

; assume after a few iterations, 'guess' is now 1.4142156862745097:
; (if (good-enough? 1.4142156862745097 2)
;     1.4142156862745097
;     (sqrt-iter (improve 1.4142156862745097 2)
;                2))

; (if #t
;     1.4142156862745097
;     (sqrt-iter (improve 1.4142156862745097 2)
;                2))

; 1.4142156862745097
; The following process shows why this works and terminates after a certain number of iterations

; HOWEVER, when we use new-if, this specific procedure does not go through normal order processing, instead it goes through applicative order

; (sqrt-new-if 2)

; (sqrt-iter-new-if 1.0 2)

; (new-if (good-enough? 1.0 2)
;         1.0
;         (sqrt-iter-new-if (improve 1.0 2)
;                           2))

; In this particular scenario all the arguments are evaluated first before evaluating the proceedure

; (new-if #f
;         1.0
;         (sqrt-iter-new-if 1.5 2))

; (new-if #f
;         1.0
;         (new-if (good-enough? 1.5 2)
;                 1.5
;                 (sqrt-iter-new-if (improve 1.5 2)
;                                   2)))

; (new-if #f
;         1.0
;         (new-if #f
;                 1.5
;                 (sqrt-iter-new-if 1.41666 2)))

; (new-if #f
;         1.0
;         (new-if #f
;                 1.5
;                 (new-if (good-enough? 1.41666 2)
;                         1.416666
;                         (sqrt-iter-new-if (improve 1.4166 2)
;                                           2))))

; This process continues, even after the predicate is evaluated to be true, leading to an infinite number of recursions


"Ex 1.7"
; When referring to very small numbers, I am assuming that it is referring to non-integer positive values less than 1.
; In such instances, the 'good-enough?' test will not work because it only takes into account errors of <= 0.001
; The ratio between the guess and the acceptable error decreases to the point where values no longer become valid

(sqrt 0.0001) ; The actual value is 0.01, but the result says 0.03230844833048122, more than 3 times the actual result

; Also, for extremely large values, this test is inadequate because the ratio between the guess and the acceptable error becomes so large, that we will be iterating too many times
; (sqrt 10000000000000000000000000000000000000000000000000000) ; Value is 1e+26, but instead of cutting the number of 0s in half, it takes a vary long time to calculate (3 minutes and going on a Raspberry Pi 4 as I type this)

(define error-frac (/ 1 1000))

(define (good-enough-new? guess x)
  (< (abs (- (/ (square guess) x) 1)) error-frac))

(define (sqrt-iter-new guess x)
  (if (good-enough-new? guess x)
      guess
      (sqrt-iter-new (improve guess x)
                     x)))
(define (sqrt-new x)
  (sqrt-iter-new 1.0 x))

(sqrt-new 2)
(sqrt-new 0.001) ; more accurate than the previous one, but still highly inaccurate
(sqrt-new 10000000000000000000000000000000000000000000000000000) ; able to calculate this extremely fast

"Ex 1.8"
(define (cube x)
  (* x x x))

(define (good-enough-cb? guess x)
  (< (abs (- (/ (cube guess) x) 1)) error-frac))

(define (improve-cb y x)
  (/ (+ (/ x (square y)) (* 2 y)) 3))

(define (cbrt-iter guess x)
  (if (good-enough-cb? guess x)
      guess
      (cbrt-iter (improve-cb guess x)
                 x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 8)
(cbrt 9)
(cbrt 27)
(cbrt 64)
(cbrt 1000000000000000000000000000000000000000000000000000000000000000) ; able to calculate this instantenously
