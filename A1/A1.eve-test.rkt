#lang racket ; CSC324 — 2022W — Assignment 1 — Eve — Design and Testing

; • Eve: Eager By-Value Environmental Evaluator for an Extended Lambda Calculus

; Task: understand the syntax and semantics of the language LCE described below,
; then create a good test suite for an evaluator eve of LCE which you will then
; implement in A1.eve.rkt

(require "A1.eve.rkt"
         rackunit)

; We'll refer to the language being interpreted in this part as “LCE”.

; · Terms in LCE

; LCE has the same terms as LCA, with the same representation.

; · Values, Closures, and Environments in LCE

; A closure pairs a (not necessarily closed) λ term with an environment that
; contains (at least) bindings for the open variables in the λ term.

; An environment pairs a local binding of a variable and value with a parent
; environment.

; For concision, closures and environments will be kept in a hash table to be
; referenced by symbolic indices of the form "λn" and "En".

; So a closure is represented by a two-element list containing a λ term and an
; environment index.

; And an environment is represented by a two-element list containing a
; two-element list of variable and value along with an environment index.

; A value is then one of:
;   closure index
;   literal term

; · Semantics of LCE

; Evaluation now takes both a term and the index of an environment that contains
; (at least) bindings for the open variables in the term.

; Evaluation of a value is still just the value itself.

; Evaluation of a variable is its most local value starting from the environment
; and continuing up the parent chain.
; Evaluation Assumption: the variable has a binding in the chain.

; Evaluation of function call is still eager and by value, but with argument
; passed by environment.
; More precisely, evaluation of (<function-term> <argument-term>) is:
;   1. Evaluate <function-term> in the environment.
;      Evaluation Assumption: the evaluation is not an infinite recursion
;      and the result is the index of a closure with λ term (λ (<id>) <body>))
;      and environment E.
;   2. Evaluate <argument-term> in the environment to produce a value v.
;      Evaluation Assumption: the evaluation is not an infinite recursion.
;   3. Produce the evaluation of <body> in a new environment extending E with
;      a local binding of <id> to v.

; Evaluation of a λ term is the index of a new closure containing the term and
; environment index.

; · Design and Testing

; Create a good test suite for eve, with a comment above each test case giving
; a rationale for its inclusion.

; Also include test cases for any significant helper functions you create
; (e.g. a recursive function is likely to be significant) to aid debugging, and
; export those helper functions from A1.eve.rkt so they can be referenced here.

; Illustrating indexer.
(define s (indexer 'p))
; A result of indexer produces successive indices starting at 0.
(check-equal? (s) 'p0)
(check-equal? (s) 'p1)
(check-equal? (s) 'p2)
; Warning: the result is not referentially transparent!
(check-not-equal? (s) (s))

; Illustrative example for the form of an environment-closure table.
; The test also illustrates the literal notation for (immutable) hash tables.
; In A1.eve.rkt you update a mutable hash table, but it's converted to immutable
; just before return (an immutable hash table and a mutable hash table are
; always considered unequal).

(check-equal? (eve '(((λ (x) (λ (y) 1)) 20) 300))
              '(1 #hash((E1 . ((x 20) E0))
                        (E2 . ((y 300) E1))
                        (λ0 . ((λ (x) (λ (y) 1)) E0))
                        (λ1 . ((λ (y) 1) E1)))))


; ************************* The test cases for the function `eva` ****************************

; A literal value should return the value with an empty environment-closure hash table.
(check-equal? (eve 1) '(1 #hash()))
(check-equal? (eve "a") '("a" #hash()))
(check-equal? (eve (+ 3 3)) '(6 #hash()))

; A lambda expression should add a closure definition to the environment-closure hash table.
(check-equal? (eve '(λ (x) (λ (y) x))) '(λ0 #hash((λ0 . ((λ (x) (λ (y) x)) E0)))))
(check-equal? (eve '((λ (x) ((λ (y) y)100)) 1)) '(100 #hash(
                                                            (λ0 . ((λ (x) ((λ (y) y) 100)) E0))
                                                            (E1 . ((x 1) E0))
                                                            (λ1 . ((λ (y) y) E1))
                                                            (E2 . ((y 100) E1))
                                                            )))
              

; A 3-layer lambda expression but with one function call should add one variable binding
; and two λ expressions to the environment-closure hash table. Also it should return the
; index of the 2nd λ closure added.
(check-equal? (eve '((λ (x) (λ (y) (λ (t) 5))) 0)) '(λ1 #hash(
                                                              (λ0 . ((λ (x) (λ (y) (λ (t) 5))) E0))
                                                              (E1 . ((x 0) E0))
                                                              (λ1 . ((λ (y) (λ (t) 5)) E1))
                                                              )))


; A 3-layer lambda expression but with two function calls should add two variable bindings
; and three λ expressions to the environment-closure hash table. Also it should return the
; index of the 3rd λ closure added.
(check-equal? (eve '(((λ (x) (λ (y) (λ (t) 5))) 0) 1)) '(λ2 #hash(
                                                                  (λ0 . ((λ (x) (λ (y) (λ (t) 5))) E0))
                                                                  (E1 . ((x 0) E0))
                                                                  (λ1 . ((λ (y) (λ (t) 5)) E1))
                                                                  (E2 . ((y 1) E1))
                                                                  (λ2 . ((λ (t) 5) E2))
                                                                  )))



; A 3-layer lambda expression, with three function calls should add three variable bindings
; and three λ expressions to the environment-closure hash table. Also it should return the
; result of the final evaluation of the whole lambda expressions.
(check-equal? (eve '((((λ (x) (λ (y) (λ (t) 5))) 0) 1) 2)) '(5 #hash(
                                                                     (λ0 . ((λ (x) (λ (y) (λ (t) 5))) E0))
                                                                     (E1 . ((x 0) E0))
                                                                     (λ1 . ((λ (y) (λ (t) 5)) E1))
                                                                     (E2 . ((y 1) E1))
                                                                     (λ2 . ((λ (t) 5) E2))
                                                                     (E3 . ((t 2) E2))
                                                                     )))



; ************************* helper functions tests *************************
(define input-env-clsr-map (make-hash))
(define expected-env-clsr-map (make-hash))


; ************************* The test cases for the helper function `find-key-for-var` ****************************

; Description of usage:
; (find-key-for-var 'x input-env-clsr-map 'E3) will find the key-value entry of the variable x
; on the hash map input-env-clsr-map, and it will start its search from the env E3. It will return
; the list of (old-environment new-extended-environment) of the var x in the map

; . Test case 1
; Given input-env-clsr-map with one key-value entry for x as 'E1 . `((x 1) E0),
; `find-key-for-var` should return a list containing the parent env E0 and
; extended new env E1.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (t) 100)))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) 100))) E0))
(hash-set! input-env-clsr-map 'E1 `((x 1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) 100)) E1))
(hash-set! input-env-clsr-map 'E2 `((y 2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (t) 100) E2))
(hash-set! input-env-clsr-map 'E3 `((t 3) E2))

; Make a copy of input-env-clsr-map to test that the function `find-key-for-var` will not mutate it
(define copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-key-for-var 'x input-env-clsr-map 'E3) (list 'E0 'E1))
(check-equal? input-env-clsr-map copied-map)


; . Test case 2
; Given input-env-clsr-map with two entries for x, the `find-key-for-var`
; should return a the list of info from the most recent scope 'E3 `((x 3) E2)
; containing the parent env E2 and extended new env E3.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (x) 100)))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-clear! copied-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) 100))) E0))
(hash-set! input-env-clsr-map 'E1 `((x 1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) 100)) E1))
(hash-set! input-env-clsr-map 'E2 `((y 2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (t) 100) E2))
(hash-set! input-env-clsr-map 'E3 `((x 3) E2))

; Make a copy of input-env-clsr-map to test that the function `find-key-for-var` will not mutate it
(set! copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-key-for-var 'x input-env-clsr-map 'E3) (list 'E2 'E3))
(check-equal? input-env-clsr-map copied-map)


; ************************* The test cases for the helper function `find-identifier-value` ****************************

; Description of usage:
; (find-identifier-value 'x input-env-clsr-map 'E3) will find the value of the identifier x
; in the input-env-clsr-map, strating to seach from the env E3

; . Test case 1
; Given input-env-clsr-map with one key-value entry for x as 'E1 . `((x 1) E0),
; `find-identifier-value` should return the value 1.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (t) 100)))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) 100))) E0))
(hash-set! input-env-clsr-map 'E1 `((x 1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) 100)) E1))
(hash-set! input-env-clsr-map 'E2 `((y 2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (t) 100) E2))
(hash-set! input-env-clsr-map 'E3 `((t 3) E2))

; Make a copy of input-env-clsr-map to check that the function `find-identifier-value` will not mutate it
(set! copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-identifier-value 'x input-env-clsr-map 'E3) 1)
(check-equal? input-env-clsr-map copied-map)


; . Test case 2
; Given input-env-clsr-map with two entries for x, `find-identifier-value`
; should return the value related to the most recent scope 'E3 `((x 3) E2)
; which is 3.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (x) 100)))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-clear! copied-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) 100))) E0))
(hash-set! input-env-clsr-map 'E1 `((x 1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) 100)) E1))
(hash-set! input-env-clsr-map 'E2 `((y 2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (t) 100) E2))
(hash-set! input-env-clsr-map 'E3 `((x 3) E2))

; Make a copy of input-env-clsr-map to check that the function `find-identifier-value` will not mutate it
(set! copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-identifier-value 'x input-env-clsr-map 'E3) 3)
(check-equal? input-env-clsr-map copied-map)



; ************************* The test cases for the helper function `add-var-to-map` ****************************

; Description of usage:
; (add-var-to-map 'λ0 1 input-env-clsr-map 'E1) where λ0 refers to
; the `((λ (x) (λ (y) (λ (x) 100))) in the input-env-clsr-map, will add the
; entry (E1 . '((x 1) λ0's parent env)) to input-env-clsr-map

; . Test case 1

; Given input-env-clsr-map with an entry for λ0, the function `add-var-to-map` should
; add the given value for its arg variable to the map. Also it sholuld return a list
; containing all the body terms and result term of the λ0.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323))))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) 100))) E0))

(hash-clear! expected-env-clsr-map)
(hash-set! expected-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) 100))) E0))
(hash-set! expected-env-clsr-map 'E1 `((x 1) E0))

(check-equal? (add-var-to-map 'λ0 1 input-env-clsr-map 'E1) `(λ (y) (λ (x) 100)))
(check-equal? input-env-clsr-map expected-env-clsr-map)


; . Test case 2

; Given input-env-clsr-map with an entry for λ1, the function `add-var-to-map` should
; add the given value for its arg variable to the map. Also it sholuld return a list
; containing all the body terms and result term of the λ1.

; Fill in the input-env-clsr-map with the entries for the term
; `(((λ (x) (λ (x) 100))1)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (x) 100)) E0))
(hash-set! input-env-clsr-map 'E1 `((x 1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (x) 100) E1))

(hash-clear! expected-env-clsr-map)
(hash-set! expected-env-clsr-map 'λ0 `((λ (x) (λ (x) 100)) E0))
(hash-set! expected-env-clsr-map 'E1 `((x 1) E0))
(hash-set! expected-env-clsr-map 'λ1 `((λ (x) 100) E1))
(hash-set! expected-env-clsr-map 'E2 `((x 3) E1))

(check-equal? (add-var-to-map 'λ1 3 input-env-clsr-map 'E2) 100)
(check-equal? input-env-clsr-map expected-env-clsr-map)











