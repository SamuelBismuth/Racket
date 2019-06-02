#lang pl

;; The Flang interpreter – supporting both the substitution model and
;; the substitution-cache model

#|//////////////////////////////|#
#|PREVIOUS ASSIGNMENT!!!!!!!!!!.|#
#|//////////////////////////////|#

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
  [IdFROL Symbol]
  [WithFROL Symbol RegE RegE]
  [Bool Boolean]
  [Geq RegE RegE]
  [Maj RegE]
  [If RegE RegE RegE]
  [FunFROL Symbol RegE]
  [CallFROL RegE RegE])
;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-list
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))]))(: parse-sexprFROL : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexprFROL sexpr)
  (match sexpr
    [(list 'reg-len = (number: n) args)
     (if (> n 0) ;; remember to make sure specified register length is at least 1
         (parse-sexpr-RegL args n)
         (error 'parse-sexprFROL "Register length must be at least 1 ~s" sexpr) )]
    [else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))
(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; to convert s-expressions into RegEs
(define (parse-sexpr-RegL sexpr reg-len)
  (match sexpr
    ['true (Bool #t)]
    ['false (Bool #f)]
    [(list (and a (or 1 0)) ... ) (if (= reg-len (length a))
                                      (Reg (list->bit-list a))
                                      (error 'parse-sexpr-RegE "wrong number of bits in ~s" a))] ;; from the slides (changing the given code).
    [(list 'and list1 list2) (And (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'or list1 list2) (Or (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'shl list) (Shl (parse-sexpr-RegL list reg-len))]
    [(symbol: id-name) (IdFROL id-name)]  
    [(cons 'with args)
     (match sexpr
       [(list 'with (list (symbol: oldName) newName) body)
        (WithFROL oldName (parse-sexpr-RegL newName reg-len) (parse-sexpr-RegL body reg-len))]
       [else (error 'parse-sexpr-RegE "bad `with' syntax in ~s" sexpr)])]
    [(list 'geq? list1 list2) (Geq (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len))]
    [(list 'maj? list) (Maj (parse-sexpr-RegL list reg-len))]
    [(list 'if list1 list2 list3) (If (parse-sexpr-RegL list1 reg-len) (parse-sexpr-RegL list2 reg-len) (parse-sexpr-RegL list3 reg-len))]
    [(list 'call fun arg) (CallFROL (parse-sexpr-RegL fun reg-len) (parse-sexpr-RegL arg reg-len))]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (FunFROL name (parse-sexpr-RegL body reg-len))]
       [else (error 'parse-sexprFROL "bad `fun' syntax in ~s" sexpr)])]
    [else (error 'parse-sexprFROL "bad syntax in ~s" sexpr)]))
(: parseFROL : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parseFROL str)
  (parse-sexprFROL (string->sexpr str)))

#|///////////////////////////////|#
#|QUESTION 2 -> about 10 minutes.|#
#|///////////////////////////////|#

;;difficulties: Nothing in particular, this question was pretty simple.

(define-type RES
  [RegV Bit-List]
  [MyBool Boolean]
  [MyFun Symbol RegE])

#|////////////////////////////|#
#|QUESTION 3 -> about 1 hours.|#
#|////////////////////////////|#

;;difficulties: At the beginning, implementing this question wasn't that hard, but
;; at the time of the test and when I had to achieve a full coverage, I need to understand better how it's work.
;; Then, after a lot of test I do success to full cover the constructor.

(: substFROL : RegE Symbol RegE -> RegE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (substFROL expr from to)
  (cases expr
    [(Reg n) expr]
    [(Bool b) expr]
    [(And l r) (And (substFROL l from to) (substFROL r from to))] 
    [(Or l r) (Or (substFROL l from to) (substFROL r from to))]
    [(Shl s) (Shl (substFROL s from to))]
    [(IdFROL name) (if (eq? name from) to expr)]
    [(WithFROL bound-id named-expr bound-body)
     (WithFROL bound-id
               (substFROL named-expr from to)
               (if (eq? bound-id from)
                   bound-body
                   (substFROL bound-body from to)))]
    [(Geq l r) (Geq (substFROL l from to) (substFROL r from to))]
    [(Maj list) (Maj (substFROL list from to))]
    [(If l m r) (If (substFROL l from to) (substFROL m from to) (substFROL r from to))]
    [(CallFROL l r) (CallFROL (substFROL l from to) (substFROL r from to))]
    [(FunFROL bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (FunFROL bound-id (substFROL bound-body from to)))]
    ))

#|////////////////////////////|#
#|QUESTION 4 -> about 2 hours.|#
#|////////////////////////////|#

;;difficulties: The implementation of all the function ask a good understanding of all the use
;; of the define we already implements, so that was a little bit hard.
;; Testing the function was actually easy since I did understand them good.

(: evalFROL : RegE -> RES)
;; evaluates RegE expressions by reducing them to bit-lists
(define (evalFROL expr)
  (cases expr
    [(Reg reg) (RegV reg)]
    [(Bool bl) (MyBool bl)]
    [(And list1 list2) (reg-arith-op bit-and (evalFROL list1) (evalFROL list2))]
    [(Or list1 list2) (reg-arith-op bit-or (evalFROL list1) (evalFROL list2))]
    [(Shl list) (RegV (shift-left (RegV->bit-list (evalFROL list))))]
    [(WithFROL bound-id named-expr bound-body)
     (evalFROL (substFROL bound-body
                          bound-id
                          named-expr))]
    [(If E1 E2 E3) (if (cases (evalFROL E1)
                         [(MyBool myBool) myBool]
                         [else #t]) (evalFROL E2) (evalFROL E3))]
    [(Maj list1) (MyBool (majority? (RegV->bit-list (evalFROL list1))))]
    [(Geq list1 list2) (MyBool (geq-bitlists? (RegV->bit-list (evalFROL list1)) (RegV->bit-list (evalFROL list2))))]
    [(IdFROL name) (error 'evalFROL "free identifier: ~s" name)]
    [(FunFROL bound-id bound-body) (MyFun bound-id bound-body)]
    [(CallFROL fun-expr arg-expr)
     (let ([fval (evalFROL fun-expr)])
       (cases fval
         [(MyFun bound-id bound-body)
          (evalFROL (substFROL bound-body
                               bound-id
                               arg-expr))]
         [else (error 'evalFROL "`call' expects a function, got: ~s"
                      fval)]))]
    ))
  
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
    [(null? bl1) #t]  ;; So is bl2.
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
    [else (error RegV->bit-list "run must return a bit-list")]))

#|///////////////////////////////|#
#|QUESTION 5 -> about 10 minutes.|#
#|///////////////////////////////|#

;;difficulties: Easy question: nothing in particular.

(: runFROL : String -> Bit-List)
;; evaluate a ROL program contained in a string
;; we will not allow to return a boolean type
(: runFROL : String -> Bit-List)
(define (runFROL str)
  (RegV->bit-list (evalFROL(parseFROL str))))

#|///////////////////////////////|#
#|CURRENT ASSIGNMENT !!!!!!!!!!!.|#
#|///////////////////////////////|#

;;difficulties: Easy question: nothing in particular.v
#|////////////////////////////|#
#|QUESTION 1 -> about 2 hours.|#
#|////////////////////////////|#

;;difficulties: Need first to understand the question, it took a lot of time since I got confused with the given code and we were supposed to do.
;; Then, need to go over the errors and fixed them one by one until there is no more error.
;; A little bit of dark work aslo took time.

;; The Flang interpreter – supporting both the substitution model and
;; the substitution-cache model

#|
The grammar:
<FLANG> ::=
|
|
|
|
|
|
|
|
<num>
{ + <FLANG> <FLANG> }
{ - <FLANG> <FLANG> }
{ * <FLANG> <FLANG> }
{ / <FLANG> <FLANG> }
{ with { <id> <FLANG> } <FLANG> }
<id>
{ fun { <id> } <FLANG> }
{ call <FLANG> <FLANG> }
|#
(define-type FLANG
  [Num Number]
  [Add FLANG FLANG]
  [Sub FLANG FLANG]
  [Mul FLANG FLANG]
  [Div FLANG FLANG]
  [Id Symbol]
  [With Symbol FLANG FLANG]
  [Fun Symbol FLANG]
  [Call FLANG FLANG])

(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)
     (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))
;;;;;;
;; the evaluation part for the substitution model
(: subst : FLANG Symbol FLANG -> FLANG)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]))
(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (: Num->number : FLANG -> Number)
  (define (Num->number e)
    (cases e
      [(Num n) n]
      [else (error 'arith-op "expects a number, got: ~s" e)]))
  (Num (op (Num->number expr1) (Num->number expr2))))
(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))][(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr]
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr)])
       (cases fval
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]))
(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [else (error 'run "evaluation returned a non-number: ~s" result)])))


;;;;;; The evaluation part for the substitution cache model
;; a type for substitution caches:
(define-type SubstCache = (Listof (List Symbol FLANG)))
(: empty-subst : SubstCache)
(define empty-subst null)
(: extend : Symbol FLANG SubstCache -> SubstCache)
(define (extend name val sc)
  (cons (list name val) sc))
(: lookup : Symbol SubstCache -> FLANG)(define (lookup name sc)
                                         (let ([cell (assq name sc)])
                                           (if cell
                                               (second cell)
                                               (error 'lookup "no binding for ~s" name))))
(: counterx : Natural)
(define counterx 0)
;;;above eval
(: evalSC : FLANG SubstCache -> FLANG)
;; evaluates FLANG expressions by reducing them to expressions
(define (evalSC expr sc)
  (set! counterx (add1 counterx))
  (if (> counterx 500)
      (error 'eval "exceeded 500 times")
      (cases expr
        [(Num n) expr]
        [(Add l r) (arith-op + (evalSC l sc) (evalSC r sc))]
        [(Sub l r) (arith-op - (evalSC l sc) (evalSC r sc))]
        [(Mul l r) (arith-op * (evalSC l sc) (evalSC r sc))]
        [(Div l r) (arith-op / (evalSC l sc) (evalSC r sc))]
        [(With bound-id named-expr bound-body)
         (evalSC bound-body
                 (extend bound-id (evalSC named-expr sc) sc))]
        [(Id name) (lookup name sc)]
        [(Fun bound-id bound-body) expr]
        [(Call fun-expr arg-expr)
         (let ([fval (evalSC fun-expr sc)])
           (cases fval
             [(Fun bound-id bound-body)
              (evalSC bound-body
                      (extend bound-id (evalSC arg-expr sc) sc))]
             [else (error 'evalSC "`call' expects a function, got: ~s" fval)]))])))

(: runSC : String -> Number)
;; evaluate a FLANG program contained in a string
(define (runSC str)
  (let ([result (evalSC (parse str) empty-subst)])
    (cases result
      [(Num n) n]
      [else (error 'runSC "evaluation returned a non-number: ~s" result)])))

#|/////////////////////////////|#
#|QUESTION 2 -> about one hour.|#
#|/////////////////////////////|#

;;difficulties: Need a deep understanding of the FLANG in general, and then, find the hint for the recursion.
;; Really nice question, I liked it.

(: countFreeSingle : FLANG Symbol -> Natural)
(define (countFreeSingle flang name)
  (cases flang
    [(Num n) 0 ]
    [(Add l r) (+ (countFreeSingle l name) (countFreeSingle r name))]
    [(Sub l r) (+ (countFreeSingle l name) (countFreeSingle r name))]
    [(Mul l r) (+ (countFreeSingle l name) (countFreeSingle r name))]
    [(Div l r) (+ (countFreeSingle l name) (countFreeSingle r name))]
    [(With bound-id named-expr bound-body) (if (eq? bound-id name) (countFreeSingle named-expr name)
                                               (+ (countFreeSingle bound-body name) (countFreeSingle named-expr name)))]
    [(Id char) (if (eq? char name) 1 0)]
    [(Fun bound-id bound-body) (if (eq? bound-id name) 0 (countFreeSingle bound-body name))]
    [(Call fun-expr arg-expr) (+ (countFreeSingle fun-expr name) (countFreeSingle arg-expr name))]
    )
  )

(: CFSingle : String Symbol -> Natural)
(define (CFSingle expr name)
  (countFreeSingle (parse expr) name))

#|Explanation|#

;; (CFSingle "{with {foo {fun {y} {+ x y}}}
;; {with {x 4}
;; {call foo 3}}}" 'x)
;; return 1

;; (run "{with {foo {fun {y} {+ x y}}}
;; {with {x 4}
;; {call foo 3}}}")
;; return 7

#|
Here is the function:
(run "{with {foo {fun {y} {+ x y}}}
{with {x 4}
{call foo 3}}}")
After calling the instance with we get:
"{with {x 4}
{call {fun {y} {+ x y}} 3}}}"
Then,
"{call {fun {y} {+ 4 y}} 3}}"
And finally:
{+ 4 3} that is 7.
So, we can see that the x is not a free identify and yes, the function run without error.

When running the function CFSingle on the same FLANG, the function returns 1.
This is explained by the x at the beginning that is considered as a free identify.
Then, the function must count the x as a free identify, but be carefull, after the running of the function with it will not be a free identify anymore!
|#

#|/////////////////////////////|#
#|QUESTION 3 -> about one hour.|#
#|/////////////////////////////|#

;;difficulties: First I wanted to implement a test case with a function that called itself.
;; I actually passed the first test but not the second...
;; After a lot of tries, I conclude that I needed a second with and then, it works bh.
;; This question too is a really nice question, I liked it.

(define loop "{with {Samuel {fun {x} {call f x}}} {with {f {fun {x} {call Samuel x}}} {call Samuel f}}}")

;; 54 tests.
(test (runFROL "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (runFROL "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (runFROL "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") =>'(0 1 0 1))
(test (runFROL "{ reg-len = 4 {1 0 0 0}}") => '(1 0 0 0))
(test (runFROL "{ reg-len = 4 {shl {1 0 0 0}}}") => '(0 0 0 1))
(test (runFROL "{ reg-len = 4 {and {shl {1 0 1 0}}{shl {1 0 1 0}}}}") =>'(0 1 0 1))
(test (runFROL "{ reg-len = 2 {and {shl {1 0}} {1 0}}}") =>'(0 0))
(test (runFROL "{ reg-len = 2 {or {0 0} {1 0}}}") =>'(1 0))
(test (runFROL "{ reg-len = 4 {with {x {1 1 1 1}} {shl y}}}")=error> "free identifier: y")
(test (runFROL "{ reg-len = 2 {with {x { or {and {shl {1 0}}{1 0}}{1 0}}}{shl x}}}") => '(0 1))
(test (runFROL "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error>"wrong number of bits in (0 1 1)")
(test (runFROL "{ reg-len = 0 {}}") =error>"Register length must be at least 1")
(test (runFROL "{ reg-len = 2 {if false {0 0} {1 1}}}") => '(1 1))
(test (runFROL "{ reg-len = 4 {if false {shl {1 0 1 1}} {shl {1 0 1 1}}}}") =>'(0 1 1 1))
(test (runFROL "{ reg-len = 4 {if {maj? {0 0 1 1}}{shl {1 0 1 1}}{1 1 0 1}}}")=> '(0 1 1 1))
(test (runFROL "{ reg-len = 4 {if false {shl {1 0 1 1}} {1 1 0 1}}}") => '(1 1 0 1))
(test (runFROL "{ reg-len = 4 {if true {shl {1 0 1 1}} {1 1 0 1}}}") => '(0 1 1 1))
(test (runFROL "{ reg-len = salut {with {x {1 1 1 1}} {shl y}}}")=error> "parse-sexprFROL: bad syntax in (reg-len = salut (with (x (1 1 1 1)) (shl y)))")
(test (runFROL "{ reg-len = 4 {with {x buuuug bug}}}")=error> "parse-sexpr-RegE: bad `with' syntax in (with (x buuuug bug))")
(test (runFROL "{ reg-len = 2 {if {maj? {0 0}}{0 1} {0 1}}}") =>'(0 1))
(test (runFROL "{ reg-len = 2 {if {geq? {0 0}{1 1}}{0 1} {0 1}}}") =>'(0 1))
(test (runFROL "{ reg-len = 2 {buuug}}")=error> "parse-sexprFROL: bad syntax in (buuug)")
(test (runFROL "{ reg-len = 2 {maj? {0 0}}}")=error> "#<procedure:RegV->bit-list>: run must return a bit-list")
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{maj? x}}}")=error> "#<procedure:RegV->bit-list>: run must return a bit-list")
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{or {0 1} {0 1}}}}") =>'(0 1))
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{and {0 1} {0 1}}}}") =>'(0 1))
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{bool buug}}}")=error> "parse-sexprFROL: bad syntax in (bool buug)")
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{geq? {0 0}{1 1}}}}")=error> "#<procedure:RegV->bit-list>: run must return a bit-list")
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{if false {1 0} {0 1}}}}")=>'(0 1))
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{if false {1 0} {0 1}}}}}")=>'(0 1))
(test (runFROL "{ reg-len = 2 {with {x { or {and {or {1 0} {1 0}}{1 0}}{1 0}}}{with {x {and {and {and {0 0} {0 0}}{1 0}}{1 0}}}{if true {1 0} {1 1}}}}}")=>'(1 0))
(test (runFROL "{ reg-len = 2 {if {geq? {0 0}{1 1}}{0 1} {0 1}}}") =>'(0 1))
(test (runFROL "{ reg-len = 2 {if {geq? {1 1}{0 0}}{0 1} {0 1}}}") =>'(0 1))
(test (runFROL "{ reg-len = 2 {if {geq? {1 0}{0 1}}{0 1} {0 1}}}") =>'(0 1))
(test (runFROL "{ reg-len = 1 {if {geq? {1}{1}}{1} {1}}}") =>'(1))
(test (runFROL "{ reg-len = 1 {if {geq? {0}{0}}{0} {0}}}") =>'(0))
(test (runFROL "{ reg-len = 1 {if {0} {0} {1}}}") =>'(0))
(test (runFROL "{ reg-len = 2 {if {with {x true} x} {0 0} {1 1}}}")=>'(0 0))
(test (runFROL "{ reg-len = 2 {if {with {x true} {with {y x} y}} {0 0} {1 1}}}")=>'(0 0))
(test (runFROL "{ reg-len = 3 {with {identity {fun {x} x}} {with {foo {fun {x} {or x {1 1 0}}}} {call {call identity foo} {0 1 0}}}}}") => '(1 1 0))
(test (runFROL "{ reg-len = 3 {with {x {0 0 1}} {with {f {fun {y} {and x y}}} {with {x {0 0 0}} {call f {1 1 1}}}}}}") => '(0 0 1))
(test (runFROL "{ reg-len = 4 {with {foo {fun {z} {if {maj? z} z {shl z}}}} {call foo {if {maj? {0 0 1 1}} {shl {1 0 1 1}} {1 1 0 1}}}}}") => '(0 1 1 1))
(test (CFSingle "{+ r r}" 'r) => 2)
(test (CFSingle "{fun {r} {+ r e}}" 'e) => 1)
(test (CFSingle "{fun {r} {+ r e}}" 'r) => 0)
(test (CFSingle "{call {fun {r} {+ r e}} {with {e {+ e r}} {fun {x} {+ e r}}}}"'r) => 2)
(test (CFSingle "{call {fun {r} {+ r e}} {with {e {+ e r}} {fun {x} {+ e r}}}}"'e) => 2)
(test (run "{call {fun {x} {+ x 1}} 4}") => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}} {call add3 1}}") => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}} {with {add1 {fun {x} {+ x 1}}} {with {x 3} {call add1 {call add3 x}}}}}") => 7)
(test (run "{with {identity {fun {x} x}} {with {foo {fun {x} {+ x 1}}} {call {call identity foo} 123}}}") => 124)
(test (run "{call {call {fun {x} {call x 1}} {fun {x} {fun {y} {+ x y}}}} 123}")=> 124)
(test (runSC loop) =error> "exceeded 500 times") ;; subst-cache model
(test (run loop) =error> "free identifier: f") ;; substitution model