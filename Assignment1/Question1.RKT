#lang pl

;; Make tests, command, syntax, indent... use the strictest type possible for the returned type

;QUESTION 1 

(: plPrefixContained : String String String String String -> Any)

(define (plPrefixContained string1 string2 string3 string4 string5)
  (cond
    [(and (> (string-length string1)  1) (string=? "pl" (substring string1 0 2))) string1]
    [(and (> (string-length string2)  1) (string=? "pl" (substring string2 0 2))) string2]
    [(and (> (string-length string3)  1) (string=? "pl" (substring string3 0 2))) string3]
    [(and (> (string-length string4)  1) (string=? "pl" (substring string4 0 2))) string4]
    [(and (> (string-length string5)  1) (string=? "pl" (substring string5 0 2))) string5]
    [else #f]
    )
  )

(test (plPrefixContained "yypl" "opl" "lpTT" "lpl" "lol") => false)

;QUESTION 2A

; check if the list contains '""' (empty string).
; check if needed to send rest or all the list at the beginning.
(: longestString : (Listof Any) -> Any)

(define (longestString myList)
  (: assignString : (Listof Any) String Any -> Any)
  ; local function assignString:
  (define (assignString myList oldString newString)
    (define nextString (if (and
                            (string? newString)
                            (< (string-length oldString) (string-length newString))
                            )
                           newString
                           oldString)
      )
    (set! oldString nextString)
    (cond
      [(null? myList) (if(eq? oldString "") #f oldString)]
      [else
       (assignString (rest myList)
                     oldString
                     (first myList)
                     )
       ]
      )   
    )
  (if (eq? myList '()) #f (assignString myList "" (first myList)))
  )

(test (longestString '(34 uuu 90)) => false)
(test (longestString '(uu 56 oooo "r" "rRR" "TTT")) => "rRR")


;QUESTION 2B

; check if the list contains '""' (empty string).
; check if needed to send rest or all the list at the beginning.
(: shortestString : (Listof Any) String -> Any)

(define (shortestString myList longestString)
  (: assignString : (Listof Any) String Any -> Any)
  ; local function assignString:
  (define (assignString myList oldString newString)
    (define nextString (if (and
                            (string? newString)
                            (> (string-length oldString) (string-length newString))
                            )
                           newString
                           oldString)
      )
    (set! oldString nextString)
    (cond
      [(null? myList) (if(eq? oldString "") #f oldString)]
      [else
       (assignString (rest myList)
                     oldString
                     (first myList)
                     )
       ]
      )   
    )
  (assignString myList longestString (first myList))
  )

(: short&long-lists : (Listof(Listof Any)) -> (Listof Any))

(define (short&long-lists myListOfList)
  ((inst map Any (Listof Any)) minmax myListOfList)
  )

(: minmax : (Listof Any) -> Any)

(define (minmax myInnerList)
  (define max (longestString myInnerList))
  (if (string? max) (list  (shortestString myInnerList max) max) (list))
  
  )

(test (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("OP" "Benny") ()))
(test (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") ()))


;QUESTION 3

(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])

(: search-stack : Symbol KeyStack -> Any)

(define (search-stack key keyedStack)
  (cases keyedStack
    [(Push symbol string keyStack) (if (eq? symbol key) string #f)]
    [(EmptyKS) #f]) 
)

(: pop-stack : KeyStack -> Any)

(define (pop-stack keyedStack)
  (cases keyedStack
    [(Push symbol string keyStack) keyStack]
    [(EmptyKS) #f]) 
  )

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

;QUESTION 4

;These comments should describe what the function
;takes as input, what it outputs, what its purpose is, and how it operates. Do
;not forget to also add your personal remarks on the process in which you
;personally came to realize the above.

 
(: is-odd? : Natural -> Boolean)
;; << Add your comments here>>
;; << Add your comments here>>
(define (is-odd? x)
  (if (zero? x)false
      (is-even? (- x 1))))
(: is-even? : Natural -> Boolean)
;; << Add your comments here>>
;; << Add your comments here>>
(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))
;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;; See explanation about the All syntax at the end of the file...
;; << Add your comments here>>
;; << Add your comments here>>
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))
;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; << Add your comments here>>
;; << Add your comments here>>
(define (all-even? lst)
  (every? is-even? lst))
;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
                                                         Boolean))
;; << Add your comments here>>
;; << Add your comments here>>
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))