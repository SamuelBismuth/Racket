#lang pl 03

#|//////////////////////////////|#
#|QUESTION 1 -> about two hours.|#
#|//////////////////////////////|#

;;difficulties: Use of the syntax of Racket and get good readability on the code.

#| BNF for the ROL language:

<ROL> ::= {reg-len = <num> <RegE> }
<RegE> ::= <Bits>       
	   | {and <RegE> <RegE>}        
	   | {or <RegE> <RegE>}        
	   | {shl <RegE>}        
	   | {with {<ID> <RegE>} <RegE> }          
	   | {<ID>}
           | {<BOOL>}
           | {if <RegE> <RegE> <RegE>}
           | {geq? <RegE> <RegE>}
           | {maj? <RegE> <RegE>}
<Bits> ::= <bit> | <bit> <Bits>
<bit>::= 1 | 0
|#

;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; RegE abstract syntax trees
(define-type RegE
  [Reg Bit-List]
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE]
  [Id Symbol]
  [With Symbol RegE RegE]
  [Bool Boolean]
  [Geq RegE RegE]
  [Maj RegE RegE]
  [If RegE RegE RegE])
;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))]))(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr
    [(list 'reg-len = (number: n) args)
     (if (> n 0) ;; remember to make sure specified register length is at least 1
         (parse-sexpr-RegL args n)
         (error 'parse-sexpr "Register length must be at least 1" sexpr) )] 
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    [(list (and a (or 1 0)) ... ) (if (= reg-len (length a))
                                      (Reg (list->bit-list a))
                                      (error 'parse-sexpr-RegE "wrong number of bits in ~s" a))] ;; from the slides (changing the given code).
    [(list 'and list1 list2) (And (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'or list1 list2) (Or (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'shl list) (Shl (parse-sexpr-RegL list reg-len))]
    [(symbol: id-name) (Id id-name)]  
    [(cons 'with args)
     (match sexpr
       [(list 'with (list (symbol: oldName) newName) body)
        (With oldName (parse-sexpr-RegL newName reg-len) (parse-sexpr-RegL body reg-len))]
       [else (error 'parse-sexpr-RegE "bad `with' syntax in ~s" sexpr)])]
    [(boolean: b) (Bool b)]
    [(list 'geq list1 list2) (Geq (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'maj list1 list2) (Maj (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'if list1 list2 list3) (If (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len) (parse-sexpr-RegL list3 reg-len))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#|///////////////////////////////|#
#|QUESTION 2 -> about 10 minutes.|#
#|///////////////////////////////|#

;;difficulties: Use of the syntax of Racket and get good readability on the code.

(define-type RES
  [MyRegE Bit-List]
  [myBool Boolean])

#|///////////////////////////////|#
#|QUESTION 2 -> about 10 minutes.|#
#|///////////////////////////////|#

;;difficulties: Use of the syntax of Racket and get good readability on the code.

(: subst : RegE Symbol RegE -> RegE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr

    [(Reg n) expr]
    [(Bool b) expr]
    [(And l r) (And (subst l from to) (subst r from to))]
    [(Or l r) (Or (subst l from to) (subst r from to))]
    [(Shl s) (Shl (subst s from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Geq l r) (Geq (subst l from to) (subst r from to))]
    [(Maj l r) (Maj (subst l from to) (subst r from to))]
    [(If l m r) (If (subst l from to)(subst m from to) (subst r from to))]   
    ))





;; tests
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") =>'(0 1 0 1))
(test (run "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (run "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (run "{ reg-len = 4{and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") =>'(0 1 0 1))
(test (run "{ reg-len = 2{ or {and {shl {1 0}} {1 0}} {1 0}}}") =>'(1 0))
(test (run "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}")=error> "free identifier: y")
(test (run "{ reg-len = 2{ with {x { or {and {shl {1 0}}{1 0}}{1 0}}}{shl x}}}") => '(0 1))
(test (run "{ reg-len = 4{or {1 1 1 1} {0 1 1}}}") =error>"wrong number of bits in (0 1 1)")
(test (run "{ reg-len = 0 {}}") =error>"Register length must be at least 1")
(test (run "{ reg-len = 3{if {geq? {1 0 1} {1 1 1}}{0 0 1}{1 1 0}}}") => '(1 1 0))
(test (run "{ reg-len = 4{if {maj? {0 0 1 1}}{shl {1 0 1 1}}{1 1 0 1}}}")=> '(0 1 1 1))
(test (run "{ reg-len = 4{if false {shl {1 0 1 1}} {1 1 0 1}}}") =>'(1 1 0 1))