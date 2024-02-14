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
(test (sum-of-squares '(3)) => 9)
(test (sum-of-squares '(1 2 3 4 5)) => 55)
(test (sum-of-squares '(-1 -2 -3 -4 -5)) => 55)
(test (sum-of-squares '(1/2 2/3 3/4 4/5 5/6)) => 9329/3600)
(test (sum-of-squares '(1000000 2000000 3000000)) => 14000000000000)
(test (sum-of-squares '(0.000001 0.000002 0.000003)) => 1.4e-11)



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

#|
Question no. 2.b.i

The grammar: 
 <PLANG> ::= {{poly <AEs>} {<AEs>}}
 <AEs> ::= <AE> | <AE> <AEs> 
 <AE> ::= <num>
         |{+ <AE> <AE>}
         |{- <AE> <AE>}
         |{* <AE> <AE>}
         |{/ <AE> <AE>}
|#

#|
Question no. 2.b.ii
|#
(define-type PLANG
  [Poly (Listof AE) (Listof AE)])

(define-type AE
  [Num Number] 
  [Add AE AE] 
  [Sub AE AE] 
  [Mul AE AE] 
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE) 
 ;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr 
    [(number: n) (Num n)] 
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) 
                            (parse-sexpr rhs))] 
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) 
                            (parse-sexpr rhs))] 
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) 
                            (parse-sexpr rhs))] 
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) 
                            (parse-sexpr rhs))] 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)])) 

(: parse : String -> PLANG) 
 ;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code
      [(cons f r1)
       (match f
         [(cons 'poly r2) (Poly (list (append (parse-sexpr (first r2)) (parse (rest r2)))) (list (append (parse-sexpr (first r1)) (parse (rest r1)))))]
      )]
      )))

; (list (list 'poly AE AE... AE) (list AE AE... AE))

(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num 1) (Num 2) (Num 3)))) 
(test (parse "{{poly } {1 2} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2))") 
(test (parse "{{poly 1 2} {} }")
      =error> "parse: at least one point is required in ((poly 1 2) ())")