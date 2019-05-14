#lang pl 02

;; In this file are some simple functions, with documentation and tests for the assignment 2 of the course Programming Language.
;; For all the method, we try to use the strictest type possible for the returned type.


#|//////////////////////////////|#
#|QUESTION 1A -> about two hour.|#
#|//////////////////////////////|#

;;difficulties: Complete understanding of the question and edge examples.

#|
<SE> ::= #\<Integer> (1)
      | <Integer> (2)
      | <String> (3)
<Integer> ::= <digit>  (4)
           | <digit><Integer> (5)
           | {string-length <String>} (6)
<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 (7)
<String> ::= {string Character} (8)
          | <lambda Character> (9)
          | {string-append <String> <String*>} (10)
          | {string-insert <String> #\<digit> <Integer>} (11)
          | {number->string <Integer>} (12)

<Character> ::= #\<digit> Character (13)
             | #\<digit>  (14)

<String*> ::= <String> <String*> (15)
            | <String> (16)

<lambda Character> ::= lambda (17)
                    | "<Integer>" (18)
|#


#|//////////////////////////////|#
#|QUESTION 1B -> about one hour.|#
#|//////////////////////////////|#

;;difficulties: Be clear and concise.

;;operators: string-length; string-append; string-insert; number->string.
#|
;; 1)
<SE> -> <Integer> (15)
<Integer> -> {string-length <String>} (6)
{string-length <String>} -> {string-length {string-insert <String> #\<digit> <Integer>}} (11)
{string-length {string-insert <String> #\<digit> <Integer>}} -> {string-length {string-insert {string-append 3 <String*>} #\<digit> <Integer>}} (9) and (10) and (18)
{string-length {string-insert {string-append 3 <String*>} #\<digit> <Integer>} -> {string-length {string-insert {string-append 3 {number->string <Integer>}} #\7 7}} (7) and (12)
{string-length {string-insert {string-append 3 {number->string <Integer>}} #\7 7}} -> {string-length {string-insert  {string-append 3 {number->string <digit>}} #\7 7}} (4)
{string-length {string-insert {string-append 3 {number->string <digit>}} #\7 7}} -> {string-length {string-insert {string-append 3 {number->string 9}} #\7 7}} (7)
{string-length {string-insert {string-append 3 {number->string 9}} #\7 7}} -> {string-length {string-insert {string-append 3 9} #\7 7} 
{string-length {string-insert {string-append 3 9} #\7 7} -> {string-length {string-insert "39" #\7 7}} 

;; 2)
<SE> -> <Integer> (1)
<Integer> -> <digit> (4)
<digit> -> 4 (7)

;; 3)
<SE> -> <Integer> (1)
<Integer> -> <digit> (4)
<digit> -> 7 (7)
|#


#|////////////////////////////////|#
#|QUESTION 2A -> about 30 minutes.|#
#|////////////////////////////////|#

;;difficulties: I didn't understand the question at the beginning, but when I did, everything became simpler.

#|
The problem demonstrated by the expression is the ambiguity that the expression represent.
Indeed, there is no rules to read the expression:
We can get that {* {+ 1 {set 2}} get} -> 6 by reading {set 1} before {set 2}.
Or {* {+ {set 1} 2} get} -> 3 by reading {set 2} before {set 1}.
Fix:
Decide an order of derivation, e.g: from left to right.
|#

#|////////////////////////////////|#
#|QUESTION 2B -> about 30 minutes.|#
#|////////////////////////////////|#

;;difficulties: edge examples to get everything fix and ambiguity in my grammar...


#|
<MAE>::= {seq <FAE>} (1)
      | {seq {set <FAE>}} (2)
      | <HAE>} (3)
<FAE>::= {* <Integer> <Integer>} (3)
      | {/ <Integer> <Integer>} (4)
      | {+ <Integer> <Integer>} (5)
      | {- <Integer> <Integer>} (6)  
<LAE>::= {* <numOrGet> <numOrGet>} (7)
      | {/ <numOrGet> <numOrGet>} (8)
      | {+ <numOrGet> <numOrGet>} (9)
      | {- <numOrGet> <numOrGet>}  (10)
<HAE>::= <LAE> (11)
      | {set <LAE>} <HAE> (11)
<digit>::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 (12)
<Integer>::= <digit> (14)
          | <digit><Integer> (15)
<numOrGet>::= <Integer> (16)
           | get (17)
|#

#|
;; 1)
<MAE> -> {seq <FAE>} (1)
{seq <FAE>} -> {seq * <Integer> <Integer>} (3)
{seq * <Integer> <Integer>} -> {seq * <digit><Integer> <digit><Integer>} (15)
{seq *<digit><Integer> <digit><Integer>} -> {seq * <digit><digit> <digit><digit>} (14)
{seq *<digit><digit> <digit><digit>} ->  {seq * 98 76} (12)

;; 2)
<MAE> -> {seq <FAE>} (1)
{seq <FAE>} -> {seq - <Integer> <Integer>} (3)
{seq - <Integer> <Integer>} -> {seq - <digit><Integer> <digit><Integer>} (15)
{seq -<digit><Integer> <digit><Integer>} -> {seq - <digit><digit> <digit><digit>} (14)
{seq -<digit><digit> <digit><digit>} ->  {seq - 54 32} (12)

;; 3)
<MAE> -> {seq {set <FAE>}} (2)
{seq  {set <FAE>}} -> {seq {set - <Integer> <Integer>}} (3)
{seq {set - <Integer> <Integer>}} ->  {seq {set - <digit><Integer> <digit><Integer>}} (15)
{seq {set - <digit><Integer> <digit><Integer>}} -> {seq {set - <digit><digit> <digit><digit>}}(14)
{seq {set - <digit><digit> <digit><digit>}} -> {seq {set - 10 23}} (12)
|#

#|///////////////////////////////|#
#|QUESTION 3 -> about 20 minutes.|#
#|///////////////////////////////|#

;;difficulties: Good understanding of the fold method.  

;; Signature for the method square.
;; The argument of the function is a number. And the return is a number.
(: square : Number -> Number)

#|
Method square
-param: Any number.
-return: the square of this number.
-time of work: about five minutes.
-difficulties: None.
|#
(define (square num)
  (* num num))

;; Signature for the method sum-of-squares.
;; The argument of the function is a List of number. And the return is a number.
(: sum-of-squares : (Listof Number) -> Number)

#|
Method sum-of-squares
-param: A list of number.
-return: The sum of all the square of the list.
-time of work: about ten minutes.
-difficulties: Understanding of the fold method.
|#
(define (sum-of-squares list)
  (foldl + 0 (map square list)))

;; 6 TESTS CASES
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(2)) => 4)
(test (sum-of-squares '(-1 2 3)) => 14)
(test (sum-of-squares '(1 2 3 -4)) => 30)
(test (sum-of-squares '(0 2 3)) => 13)
(test (sum-of-squares '(0 0 3)) => 9)

#|//////////////////////////////|#
#|QUESTION 4A -> about one hour.|#
#|//////////////////////////////|#

;;difficulties: Use of the syntax of Racket and get good readability on the code.

;; Signature for the method create-polynomial.
;; The argument of the function is a List of number. And the return is a method which receive a number as argument and return number.
(: create-polynomial : (Listof Number) -> (Number -> Number))

#|
Method create-polynomial
-param: A list of number.
-return: The polyX method.
-time of work: about 30 minutes.
|#
(define (create-polynomial coeffs)
  
  ;; Signature for the method poly.
  ;; The argument of the function is a List of number a number and integer and a number. And the return a number.
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if ( null? argsL )
        accum
        (poly (rest argsL) x (+ 1 power) (+ accum (* (first argsL) (expt x power))) )))
  
  ;; Signature for the method polyX.
  ;; The argument of the function is a number. And the return a number.
  (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0))
  polyX)

#|///////////////////////////////|#
#|QUESTION 4B -> about two hours.|#
#|///////////////////////////////|#

;;difficulties: Understanding the given code and complete it without forgetting any edge example.

;; i)
#|
<PLANG>::={{poly <AEs>} {<AEs>}}
<AEs>::=<AE> | <AE> <AEs>
<AE>::= <num>
 | { + <AE> <AE> }
 | { - <AE> <AE> }
 | { * <AE> <AE> }
 | { / <AE> <AE> }
|#

;; ii)

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
    [(number: n)
     (Num n)]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs)
                            (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s"
                 sexpr)]))
(: parse : String -> PLANG)
;; parses a string containing a PLANG expression to a PLANG AST
(define (parse str)
  (let ([code (string->sexpr str)])
    (match code
      [(list (list 'poly) rhs) (error 'parse "at least one coefficient is required in ~s" code)]
      [(list (list 'poly var var2 ...) (list var3 var4 ...)) (Poly (map parse-sexpr (cons var var2)) (map parse-sexpr (cons var3 var4)))]
      [(list (list 'poly var var2 ...) '()) (error 'parse "at least one point is required in ~s" code)]
      [else (error 'parse-sexpr "bad 'poly' syntax in ~s" code)]
      )))

;; 6 TESTS CASES
(test (parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }")=error> "parse: at least one coefficient is required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")=error> "parse: at least one point is required in ((poly 1 2) ())")
(test (parse "{{+ 4 2} { 1} }") =error> "bad 'poly' syntax in ((+ 4 2) (1))")
(test (parse "{+ 1 2}") =error> "bad 'poly' syntax in (+ 1 2)")
(test (parse "{{poly 3 1 2} {4 5 12}}") => (Poly (list (Num 3) (Num 1) (Num 2)) (list (Num 4) (Num 5) (Num 12))))

;; iii)

;; evaluates AE expressions to numbers
;;Input: AE 
;;Output: Number
(: eval : AE -> Number) 
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]))

;; Signature for the method eval-poly.
;; The argument of the function is a PLANG. And the return a list of number.
(: eval-poly : PLANG -> (Listof Number))

#|
Method eval-poly
-param: A polynom.
-return: A list of the coeffs.
-time of work: about five minutes.
|#
(define (eval-poly p-expr)
  (cases p-expr [(Poly lc lp) (map (create-polynomial (map eval lc)) (map eval lp))]))

;; Signature for the method run.
;; The argument of the function is a String. And the return a list of number.
(: run : String -> (Listof Number))

#|
Method eval-poly
-param: A string.
-return: A list of the coeffs.
-time of work: about five minutes.
|#
(define (run str)
  (eval-poly (parse str)))

;; 8 TESTS CASES
(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}")=> '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}")=> '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}")=> '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}")=> '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}")=> '(0 4 4))
(test (run "{{poly {- 1 0} {+ 1 0} 0} {-1 {/ 3 1} {* 3 1}}}")=> '(0 4 4))
(test (run "{{poly {- 0 0} {+ 1 1} 0} {-1 {/ 1 1} {* 3 1}}}")=> '(-2 2 6))



