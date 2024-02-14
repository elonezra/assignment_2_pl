#lang pl 02
#|
Ex 1
|#
(: exp2 : Number -> Number)
(define (exp2 x)
  (* x x))
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lst)
(foldl + 0 (map exp2 lst))
  )

(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(1 -2 3)) => 14)
(test (sum-of-squares '(1 2 -3)) => 14)
(test (sum-of-squares '(1/2 2 3)) => 53/4)
(test (sum-of-squares '()) => 0)