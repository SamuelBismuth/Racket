#lang pl

;; In this file are some simple functions, with documentation and tests for the assignment 1 of the course Programming Language.
;; For all the method, we try to use the strictest type possible for the returned type.


#|/////////////////////////////////////////////////////////////|#
#|QUESTION 1 -> about one hour (including tests and debugging).|#
#|/////////////////////////////////////////////////////////////|#

;; My solution for this question was to use a cond statement and to check all the string one by one.


;; Signature for the method plPrefixContained.
;; The arguments of the function are five string. And the return is Any type.
(: plPrefixContained : String String String String String -> Any)

#|
Method plPrefixContained:
-param: string1 string2 string3 string4 string5: five strings without any restriction.
-return: if one string contains "pl" at the beginning of the word, return the string, else return false.
-time of work: about thirty minutes.
-difficulties: use of the cond statement and understanding how the racket language (at his basic) works.
|#
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

;; 6 TESTS CASES
(test (plPrefixContained "yypl" "opl" "lpTT" "lpl" "lol") => false)
(test (plPrefixContained "" "" "" "" "") => false)
(test (plPrefixContained "pl" "pl" "pl" "pl" "pl") => "pl")
(test (plPrefixContained "plplplplplp" "plplplplplp" "plplplplplp" "plplplplplp" "plplplplplp") => "plplplplplp")
(test (plPrefixContained "yypl" "opl" "lpTT" "lpl" "pl") => "pl")
(test (plPrefixContained "yypl" "opl" "plot" "lpl" "lol") => "plot")


#|///////////////////////////////////////////////////////////////|#
#|QUESTION 2A -> about two hours (including tests and debugging).|#
#|///////////////////////////////////////////////////////////////|#

;; My solution for this question was to use a tail-recursion and an helper method.
;; The main goal is to send one by one all the element of the list and store the longer string into the variable old string.
;; The variable oldString is initialized as false and may stay false all the running if there is no string in the list.
;; At the end of the running, return the best between the oldString and the last newString.


;; Signature for the method longestString.
;; The argument of the function is a list of any type. And the return is Any type.
(: longestString : (Listof Any) -> Any)

#|
Method longestString:
-param: list of any: a sentence of any type.
-return: the longest string of the list -> if there is no string on the list: return false.
-time of work: about one hour and half.
-difficulties: Use of the tail-recursion and define type.
|#
(define (longestString myList)
  
  ;; Signature for the method assignString.
  ;; The argument of the function is a list of any type and two Any types. And the return is Any type.
  (: assignString : (Listof Any) Any Any -> Any)

  ;;Local method assignString:
  ;;-param: list of any: a sentence of any type, oldString: the best actual string (may be false), newString: the newString we need to test.
  ;;-return: oldString if nothing changed, or newString, if the newString is longer that the old one.
  (define (assignString myList oldString newString)
    (set! oldString (nextString oldString newString))
    (cond
      [(null? myList) (if(eq? oldString #f) #f oldString)]
      [else
       (assignString (rest myList)
                     oldString
                     (first myList)
                     )
       ]
      )   
    )
  (if (eq? myList '()) #f (assignString myList #f (first myList)))
  )

;; Signature for the method nextString.
;; The arguments of the function are two any types. And the return is Any type.
(: nextString : Any Any -> Any)

#|
Method longestString:
-param: oldString and newString (see above for furthers details).
-return: oldString if nothing changed, or newString, if the newString is longer that the old one..
-time of work: about fifteen minutes.
-difficulties: understanding how to use any type as a string.
|#
(define (nextString oldString newString)  
  (cond
    [(and (not (string? oldString)) (not(string? newString))) #f]
    [(and (string? oldString) (not(string? newString))) oldString]
    [(and (not (string? oldString)) (string? newString)) newString]
    [(>= (string-length oldString) (string-length newString)) oldString]
    [else newString]
    )
  )

;; 14 TESTS CASES
(test (longestString '(34 uuu 90)) => false)
(test (longestString '("34" uuu 90)) => "34")
(test (longestString '(34 "uuu" 90)) => "uuu")
(test (longestString '(34 uuu "90")) => "90")
(test (longestString '()) => false)
(test (longestString '(uu 56 oooo "r" "rRR" "TTT")) => "rRR")
(test (longestString '(uu 56 oooo "r" "rRR" "TTTT")) => "TTTT")
(test (longestString '(uu 56 "oooo" "r" "rRR" "TTT")) => "oooo")
(test (longestString '(uu 56 oooo r rRR TTT)) => false)
(test (longestString '(uu 56 oooo "" rRR TTT)) => "")
(test (longestString '(uu 56 oooo "" v)) => "")
(test (longestString '(uu 56 oooo "1" v)) => "1")
(test (longestString '("")) => "")
(test (longestString '("100")) => "100")


#|/////////////////////////////////////////////////////////////////|#
#|QUESTION 2B -> about three hours (including tests and debugging).|#
#|/////////////////////////////////////////////////////////////////|#

;; My solution for this question was to use the previous method (from the question 2A).
;; That is, I built a new function really similar to the "longestString" method: shortestString.
;; Thus, I built another function which iterate all the list and for each list, return the min and max string or an empty list.

;; Signature for the method shortestString.
;; The arguments of the function are list of any and a string. And the return is Any type.
(: shortestString : (Listof Any) String -> Any)

#|
Method shortestString
This method is similar to longestString so please, see it for further details.
|#
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
      [(null? myList) oldString]
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

;; Signature for the method short&long-lists.
;; The arguments of the function are list of list of Any. And the return is a list of Any.
(: short&long-lists : (Listof(Listof Any)) -> (Listof Any))

#|
Method short&long-lists:
-param: myListOfList: this is a list of list (matrix, 2D vector) of any type.
-return: a list including either an empty list, or a list with two words: the min and the max string of the item.
-time of work: about one hour and half.
-difficulties: understanding how to use map and inst.
!!! Here is one of the limitation of the racket language. !!!
EXPLANATION: At the beginning, I try to use map like the next example showing up:
(define (square x) (expt x 2))
(map square '(1 2 3 4 5)) ; '(1 4 9 16 25)
from: https://beautifulracket.com/explainer/lists.html
BUT: I got an exception. After a lot of searches, I found that one of racket limitation was that it can't use
map for a list of list: Type inference for polymorphic functions from the next website: https://docs.racket-lang.org/ts-guide/caveats.html
at 8.2.
The solution was to use "inst".
|#
(define (short&long-lists myListOfList)
  ((inst map Any (Listof Any)) minmax myListOfList)
  )

;; Signature for the method minmax.
;; The arguments of the function are list of Any. And the return is any type.
(: minmax : (Listof Any) -> Any) 

#|
Method minmax:
-param: myInnerList: this is a list of any type.
-return: either an empty list, or a list with two words: the min and the max string of myInnerList.
-time of work: about thirty minutes.
-difficulties: no particular difficulties on this method.
|#
(define (minmax myInnerList)
  (define max (longestString myInnerList))
  (if (string? max) (list  (shortestString myInnerList max) max) (list))
  )

;; 8 TESTS CASES
(test (short&long-lists '((any "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("OP" "Benny") ()))
(test (short&long-lists '(("2 5 5" 1 "5gg" L) (v gggg "f") ())) => '(("5gg" "2 5 5") ("f" "f") ()))
(test (short&long-lists '((any "" 10 "" 8) (any Benny OP (2 3)))) => '(("" "") ()))
(test (short&long-lists '(("" "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("" "Benny") ()))
(test (short&long-lists '((any) (any Benny OP (2 3)))) => '(() ()))
(test (short&long-lists '((any "Benny" 10 "OP" 8) (any "Benny" 10 "OP" 8) (any "Benny" 10 "OP" 8) (any "Benny" 10 "OP" 8) (any Benny OP (2 3)))) => '(("OP" "Benny") ("OP" "Benny") ("OP" "Benny") ("OP" "Benny") ()))
(test (short&long-lists '(())) => '(()))
(test (short&long-lists '()) => '())


#|/////////////////////////////////////////////////////////////|#
#|QUESTION 3 -> about one hour (including tests and debugging).|#
#|/////////////////////////////////////////////////////////////|#

;; My solution for this question was to use define-type.
;; That is, I built a new type named KeyStack including two fields: EmptyKS and Push.
;; Then, tI implemented the function search and pop without to much difficulty.

;; new define-type.
;; EmptyKs: an empty stack.
;; Push: including a symbol, a String and a KeyStack.
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])

;; Signature for the method search-stack.
;; The arguments of the function are Symbol and KeyStack. And the return is any type.
(: search-stack : Symbol KeyStack -> Any)

#|
Method search-stack
-param: key: the key associate with the string, keyedStack the KeyStack to make the search.
-return: The string paired with the symbol, and if the symbol doesn't exist, false.
-time of work: about twenty minutes.
-difficulties: the use of the recursion.
|#
(define (search-stack key keyedStack)
  (cases keyedStack
    [(Push symbol string keyStack) (if (eq? symbol key) string (search-stack key keyStack))] 
    [(EmptyKS) #f]) 
  )

;; Signature for the method pop-stack.
;; The argument of the function is a KeyStack. And the return is any type.
(: pop-stack : KeyStack -> Any)

#|
Method pop-stack
-param: keyedStack the KeyStack to make the pop.
-return: the keyStack after the pop action or false if the keyedStack is emtpy.
-time of work: about ten minutes.
-difficulties: no particular difficulties.
|#
(define (pop-stack keyedStack)
  (cases keyedStack
    [(Push symbol string keyStack) keyStack]
    [(EmptyKS) #f]) 
  )

; 10 TESTS CASES
(test (EmptyKS) => (EmptyKS)) 
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (search-stack 'a (Push 'c "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "A")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'c (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)

#|/////////////////////////////////////////////////////|#
#|QUESTION 4 -> about thirty minutes (only adding doc).|#
#|/////////////////////////////////////////////////////|#

;; These comments should describe what the function
;; takes as input, what it outputs, what its purpose is, and how it operates. Do
;; not forget to also add your personal remarks on the process in which you
;; personally came to realize the above.

;; Signature for the method is-odd?.
;; The argument of the function is a Natural. And the return is a Boolean.
(: is-odd? : Natural -> Boolean)

#|
Method is-odd?
-param: x: a number.
-return: true if the number is odd, and false if the number is even (including zero).
To do such a thing, the function first check if x = 0 (particular case).
Then, it's send to the function "is-even" the param "x - 1".
is-even do the same. Then, x is sended from one function to the other until it's reach 0: it must reach 0 since the function
always take one from x.
If the case "x=0" happens in the is-even method, return true, else return false.
|#
(define (is-odd? x)
  (if (zero? x) false
      (is-even? (- x 1))))

;; Signature for the method is-even?.
;; The argument of the function is a Natural. And the return is a Boolean.
(: is-even? : Natural -> Boolean)

#|
Method is-even?
-param: x: a number.
-return: true if the number is even, and false if the number is odd (including zero).
This function can't be used without the is-odd? method.
Both depends on each other.
The idea is the same we explained in the function is-odd?.
|#
(define (is-even? x)
  (if (zero? x) true
      (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))  ;;particular case: 0. is even.
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

;; Signature for the method every?.
;; The arguments of the function are: a method defined with a type A, and return a boolean and a list of A. And the return is a Boolean.
(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))

#|
Method every?
-param: pred: a method which check anything in a number (can be: is-even or is-odd as example), lst: a list of type A, must be
the same type the the param of the method pred.
-return: true if all the A type of the lst pass the test given by the method pred, else false.
The function check first if the lst is null: if yes, return true.
Else, check if all the item of the list pass the test of the function pred, if yes return true.
All the other cases are considering as false.
!!! ATTENTION, about the "All" use: the type of the item of the list must be the same that the type of the param of the pred method.
|#
(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))

;; An example for the usefulness of this polymorphic function.
;; Signature for the method all-even?.
;; The arguments of the function are: list of Natural and the return is a Boolean.
(: all-even? : (Listof Natural) -> Boolean)

#|
Method all-even?
-param: lst: a list of Natural number.
-return: the return of the function is-even using every?
See every? for further details.
Here is shown the usefulness of the every? function: it's readable and really effective to use it!
In two line, all the list is checked and more than this: everything is easily tunable.
|#
(define (all-even? lst)
  (every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

;; Signature for the method every2?.
;; The arguments of the function are: two method with as param A and B and as return Boolean, two lists one of type A,
;; and one of type B, and the return is a Boolean.
(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))

#|
Method every2?
-param: pred1: a method (can be: is-even or is-odd as example), pred2: another method (with param B this time), lst1: a list of type A, must be
the same type the the param of the method pred, and list2: same as list1 but with type B this tie.
-return: true if all the lst1 pass the test of pred1 AND all the item of the lst2 pass the test of pred2, else false.
To do this: the function check first if the lst is null: if yes, return true. (By checking that lst1 is null, we also check if lst2 is null
since we assume both list to be of same length.
Else, check if the next item of both lists pass their test, if yes go to next step (recursion), else return false.
|#
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1) ;; both lists assumed to be of same length
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))
