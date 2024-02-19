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

#|
The `parse` function parses a string containing a PLANG expression.

Parameters:
- `str`: A string representing a PLANG expression.

Return Value:
- The function returns a `Poly` object that represents the parsed PLANG expression.

Example Usage:
- `(parse "{{poly 1 2 3} {1 2 3}}")` parses the string into a `Poly` object that represents the polynomial 1 + 2x + 3x^2 evaluated at the points 1, 2, and 3.

|#
(: parse : String -> PLANG)
;; parses a string containing a PLANG expression
(define (parse str) 
 (let ([code (string->sexpr str)]) 
 (match code
   [(cons (cons 'poly coeffs) (list points))
    (cond
      [(null? coeffs) (error 'parse "at least one coefficient is required in ~s" code)]
      [(not (list? points)) (error 'parse "bad syntax in ~s" code)]
      [(null? points) (error 'parse "at least one point is required in ~s" code)]
      [else (Poly (map parse-sexpr coeffs) (map parse-sexpr points))])]
   [else (error 'parse "bad syntax in ~s" code)])))

; (list (list 'poly AE AE... AE) (list AE AE... AE))
; Test when coeffs is not a list

(test (parse "{{poly 1 2 3} {1 2 3}}")
      => (Poly (list (Num 1) (Num 2) (Num 3))
               (list (Num 1) (Num 2) (Num 3))))

(test (parse "{{poly } {1 2} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2))")

(test (parse "{{poly 1 2} {} }")
      =error> "parse: at least one point is required in ((poly 1 2) ())")

; Test when coeffs is not a number
(test (parse "{{poly '1'} {1 2 3}}")
      =error> "parse-sexpr: bad syntax in \"1\"")

; Test when points is not a list
(test (parse "{{poly 1 2 3} 1}")
      =error> "parse: bad syntax in ((poly 1 2 3) 1)")

; Test when the input does not match the expected form
(test (parse "{{1 2 3} {1 2 3}}")
      =error> "parse: bad syntax in ((1 2 3) (1 2 3))")

; Test when the input does not match the expected form
(test (parse "{{poly 1 2 3} 1 2 3}")
      =error> "parse: bad syntax in ((poly 1 2 3) 1 2 3)")

; Test when the input does not match the expected form
(test (parse "{1 2 3 1 2 3}")
      =error> "parse: bad syntax in (1 2 3 1 2 3)")

(test (parse "{{poly 4/5} {1/2 2/3 3}}")
      => (Poly (list (Num 4/5)) (list (Num 1/2) (Num 2/3) (Num 3))))

(test (parse "{{poly 2 3} {4}}")
      => (Poly (list (Num 2) (Num 3)) (list (Num 4))))

(test (parse "{{poly 1 1 0} {-1 3 3}}")
      => (Poly (list (Num 1) (Num 1) (Num 0)) (list (Num -1) (Num 3) (Num 3))))

(test (parse "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}")
      => (Poly (list (Div (Num 4) (Num 2)) (Sub (Num 4) (Num 1))) (list (Sub (Num 8) (Num 4)))))

(test (parse "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}")
      => (Poly (list (Add (Num 0) (Num 1)) (Num 1) (Mul (Num 0) (Num 9))) (list (Sub (Num 4) (Num 5)) (Num 3) (Div (Num 27) (Num 9)))))

; Test invalid expressions
(test (parse "{{poly } {1 2 3} }")
      =error> "parse: at least one coefficient is required in ((poly) (1 2 3))")

(test (parse "{{poly 4/5 } {1/2 2/3 3} {poly 1 2 4} {1 2}}")
      =error> "parse: bad syntax in ((poly 4/5) (1/2 2/3 3) (poly 1 2 4) (1 2))")

(test (parse "{{poly 2 3} {}}")
      =error> "parse: at least one point is required in ((poly 2 3) ())")

; Test when points list is empty
(test (parse "{{poly 1 1 3} }")
      =error> "parse: bad syntax in ((poly 1 1 3))")

#|
Question no. 2.b.iii
|#
;; evaluates AE expressions to numbers
(: eval : AE -> Number)
 (define (eval expr)
   (cases expr 
     [(Num n) n] 
     [(Add l r) (+ (eval l) (eval r))] 
     [(Sub l r) (- (eval l) (eval r))] 
     [(Mul l r) (* (eval l) (eval r))] 
     [(Div l r) (/ (eval l) (eval r))]))

#|
The `eval-poly` function evaluates a PLANG expression to a list of numbers.

Parameters:
- `p-expr`: A `Poly` object representing a PLANG expression.

Return Value:
- The function returns a list of numbers that are the values of the polynomial at the points specified in the `Poly` object.

Example Usage:
- `(eval-poly (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))` evaluates the polynomial 1 + 2x + 3x^2 at the points 1, 2, and 3, and returns the list `(6 17 34)`.

|#
(: eval-poly : PLANG -> (Listof Number) ) 
 (define (eval-poly p-expr)
   (cases p-expr
     [(Poly coeffs exp)
      (map (createPolynomial (map eval coeffs)) (map eval exp))]
     )
   )

#|
The `run` function evaluates a PLANG program contained in a string.

Parameters:
- `str`: A string representing a PLANG program.

Return Value:
- The function returns a list of numbers that are the results of evaluating the PLANG program.

Example Usage:
- `(run "{{poly 1 2 3} {1 2 3}}")` evaluates the PLANG program that represents the polynomial 1 + 2x + 3x^2 evaluated at the points 1, 2, and 3, and returns the list `(6 17 34)`.

|#
(: run : String -> (Listof Number))
 ;; evaluate a FLANG program contained in a string
(define (run str)
  (eval-poly (parse str)))

(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))

(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589))

(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))

(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5))

(test (run "{{poly 2 3} {4}}") => '(14))

(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))

(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => '(14))

(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4))

; Test when coeffs and points are arithmetic expressions
(test (run "{{poly {+ 1 2} {- 3 4}} {5 6 7}}") => '(-2 -3 -4))

; Test when coeffs and points are arithmetic expressions with division
(test (run "{{poly {/ 8 4} {/ 6 2}} {1 2 3}}") => '(5 8 11))

; Test when coeffs and points are arithmetic expressions with multiplication
(test (run "{{poly {* 1 2} {* 3 4}} {5 6 7}}") => '(62 74 86))

; Test when coeffs and points are arithmetic expressions with subtraction
(test (run "{{poly {- 1 2} {- 3 4}} {5 6 7}}") => '(-6 -7 -8))

; Test when coeffs list is empty
(test (run "{{poly } {1 2 3}}")
      =error> "parse: at least one coefficient is required in ((poly) (1 2 3))")

; Test when points list is empty
(test (run "{{poly 1 2 3} {}}")
      =error> "parse: at least one point is required in ((poly 1 2 3) ())")

; Test when coeffs and points are fractions
(test (run "{{poly {/ 1 2} {/ 3 4}} {5 6 7}}") => '(17/4 5 23/4))

; Test when coeffs are fractions and points are integers
(test (run "{{poly {/ 1 2} {/ 3 4}} {1 2 3}}") => '(5/4 2 11/4))

