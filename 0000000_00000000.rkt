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

; Question no. 2
#|
The `createPolynomial` function creates a polynomial function from a list of coefficients.

Parameters:
- `coeffs`: A list of numbers representing the coefficients of the polynomial. The nth number in the list represents the coefficient of the nth power of x in the polynomial.

Return Value:
- The function returns a function that takes a number `x` and returns the value of the polynomial at `x`.

Example Usage:
- `(define p (createPolynomial '(2 3 4)))` creates a polynomial function `p` that represents the polynomial 2 + 3x + 4x^2.
- `(p 5)` evaluates the polynomial at `x = 5`.
|#
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Integer -> Number)
  (define (poly argsL x power accum)
    (if (eq? accum (- power 1))
        0
        (+ (* (first argsL) (expt x (+ 1 accum))) (poly (rest argsL) x power (+ 1 accum)))))
  (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x (length coeffs) -1))
  polyX)


; Tests for question 2
(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>
(+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5
(expt 0 3))))
(test (p2345 4) =>
(+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5
(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4
(expt 11 2)) (* 5 (expt 11 3))))
(test (p2345 -4) => (+ (* 2 (expt -4 0)) (* 3 (expt -4 1)) (* 4
(expt -4 2)) (* 5 (expt -4 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6
(expt 11 2))))
(test (p536 -11) => (+ (* 5 (expt -11 0)) (* 3 (expt -11 1))
(* 6 (expt -11 2))))

(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)
(test (p_0 -1) => 0)

(define p1 (createPolynomial '(1)))
(test (p1 0) => 1)
(test (p1 1) => 1)
(test (p1 -1) => 1)
