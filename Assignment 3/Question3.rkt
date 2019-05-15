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
           | {maj? <RegE>}
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
  [Maj RegE]
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
         (error 'parse-sexpr "Register length must be at least 1 ~s" sexpr) )]
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
    [(list 'geq? list1 list2) (Geq (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'maj? list) (Maj (parse-sexpr-RegL list reg-len))]
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
  [RegV Bit-List]
  [MyBool Boolean])

#|////////////////////////////|#
#|QUESTION 3 -> about 1 hours.|#
#|////////////////////////////|#

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
    [(Maj list) (Maj (subst list from to))]
    [(If l m r) (If (subst l from to) (subst m from to) (subst r from to))]   
    ))

#|////////////////////////////|#
#|QUESTION 4 -> about 2 hours.|#
#|////////////////////////////|#

;;difficulties: Use of the syntax of Racket and get good readability on the code.

(: eval : RegE -> RES)
;; evaluates RegE expressions by reducing them to bit-lists
(define (eval expr)
  (cases expr
    [(Reg reg) (RegV reg)]
    [(Bool bl) (MyBool bl)]
    [(And list1 list2) (reg-arith-op bit-and (eval list1) (eval list2))]
    [(Or list1 list2) (reg-arith-op bit-and (eval list1) (eval list2))]
    [(Shl list) (RegV (shift-left (RegV->bit-list (eval list))))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (cases (eval named-expr)
                    [(RegV reg) (Reg reg)]
                    [(MyBool b) (Bool b)])))]
    [(If E1 E2 E3) (if (cases (eval E2)
                         [(MyBool myBool) myBool]
                         [else #t]) (eval E2) (eval E3))]
    [(Maj list1) (MyBool (majority? (RegV->bit-list (eval list1))))]
    [(Geq list1 list2) (MyBool (geq-bitlists? (RegV->bit-list (eval list1)) (RegV->bit-list (eval list2))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))
  
;; Defining functions for dealing with arithmetic operations
;; on the above types
(: bit-and : BIT BIT -> BIT) ;; Arithmetic and
(define(bit-and a b)
  (cond
    [(eq? a 1) (if (eq? b 1) 1 0)]
    [else 0])) ;; not used actually.
(: bit-or : BIT BIT -> BIT) ;; Aithmetic or
(define(bit-or a b)
  (cond
    [(eq? a 0) (if (eq? b 0) 0 1)]
    [else 1])) ;; not used actually.
(: reg-arith-op : (BIT BIT -> BIT) RES RES -> RES)
;; Consumes two registers and some binary bit operation 'op',
;; and returns the register obtained by applying op on the
;; i'th bit of both registers for all i.
(define(reg-arith-op op reg1 reg2)
  (: bit-arith-op : Bit-List Bit-List -> Bit-List)
  ;; Consumes two bit-lists and uses the binary bit operation 'op'.
  ;; It returns the bit-list obtained by applying op on the
  ;; i'th bit of both registers for all i.
  (define(bit-arith-op bl1 bl2)
    (map op bl1 bl2))
  (RegV (bit-arith-op (RegV->bit-list reg1) (RegV->bit-list reg2))))
(: majority? : Bit-List -> Boolean)
;; Consumes a list of bits and checks whether the
;; number of 1's are at least as the number of 0's.
(define(majority? bl)
  (if (>= (foldl + 0 bl) (/ (length bl) 2)) #t #f))
(: geq-bitlists? : Bit-List Bit-List -> Boolean)
;; Consumes two bit-lists and compares them. It returns true if the
;; first bit-list is larger or equal to the second.
(define (geq-bitlists? bl1 bl2)
  (cond
    [(null? bl1) (if (null? bl2) #t #f)]
    [(null? bl2) #t]
    [(eq? (first bl1) 1) (if (eq? (first bl2) 1) (geq-bitlists? (rest bl1) (rest bl2)) #t)]
    [(eq? (first bl2) 1)  #f]
    [else (geq-bitlists? (rest bl1) (rest bl2))]))

(: shift-left : Bit-List -> Bit-List)
;; Shifts left a list of bits (once)
(define(shift-left bl)
  (append (rest bl) (list (first  bl))))
(: RegV->bit-list : RES -> Bit-List)
;; extract a bit-list from RES type
(define (RegV->bit-list res)
  (cases res
    [(RegV bl) bl]
    [(MyBool myBool) (error RegV->bit-list "run must return a bit-list ~s" myBool)]))

#|///////////////////////////|#
#|QUESTION 5 -> about 1 hour.|#
#|///////////////////////////|#

;;difficulties: Use of the syntax of Racket and get good readability on the code.

(: run : String -> Bit-List)
;; evaluate a ROL program contained in a string
;; we will not allow to return a boolean type
(: run : String -> Bit-List)
(define (run str)
  (RegV->bit-list (eval(parse str))))

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