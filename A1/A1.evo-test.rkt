#lang racket ; CSC324 — 2022W — Assignment 1 - Evo Design and Testing

; • Evo: Eager By-Value Stateful Evaluator for an Extended Lambda Calculus

; Task: understand the syntax and semantics of the language LCO described below,
; then create a good test suite for an evaluator of LCO named evo which you will
; then implement in A1.evo.rkt

(require "A1.evo.rkt" 
         rackunit)

; We'll refer to the language being interpreted in this part as “LCO”.

; · Terms in LCO

; The grammar of “terms” [along with how we'll refer to them] in LCO is:
;   term = (λ (<parameter-identifier>) <body-term> ... <result-term>) [“λ term”]
;        | (<function-term> <argument-term>)  [“function call term”]
;        | (set! <variable-identifier> <update-term>)  [“assignment term”]
;        | <identifier>  [“variable term”]
;        | <literal>  [“literal term”]

; · Values, Closures, and Environments in LCO

; LCO has the same values, closures, and environments, as LCE except that
; binding values in environments are now stored inside mutable boxes
; (a datatype for replaceable values).

(check-true (box? #&108)) ; a literal (but immutable) box
(check-equal? (box 108) #&108) ; the mutable box constructor from a value
(check-equal? (unbox #&108) 108) ; extracting the value from a box
(check-equal? (let ((b (box 108)))
                (set-box! b 324)
                (unbox b))
              324)

; Notice that racket procedures are valid literal terms in LCA and LCE/LCO:
#;(check-equal? (eva add1) add1)
#;(check-equal? (eve add1) `(,add1 #hash()))
#;(check-equal? (evo add1) `(,add1 #hash()))

; We'll give a semantics to function call where the function term evaluates to
; a racket procedure. A convenient way to embed a procedure in a term is to use
; “quasiquotating” which is like quoting but can be escaped out of (“unquoted”).
; The quasiquoting character is the backwards single-quote, aka back-tick, and
; the unquoting character is the upside-down quasiquote, aka comma:
(check-equal? '(add1 2) (list 'add1 2))
(check-not-equal? '(add1 2) (list add1 2))
(check-equal? '(add1 2) `(add1 2)) ; no change since no unquoting
(check-true (symbol? (first `(add1 2))))
(check-equal? `(,add1 2) (list add1 2)) ; use variable's value
(check-true (procedure? (first `(,add1 2))))

; · Semantics of LCO

; Evaluation still takes a term and an index of an appropriate environment.

; Evaluation of a value or λ is as for LCE.

; Evaluation of a variable is as for LCE (with the extra step of extracting
; its current value from the box it is stored in).

; Evaluation of function call allows the value of the function term to be a
; racket procedure, in which case the evaluation is the result of calling that
; procedure on the value of the argument term.

; Evaluation of function call when the value of the function term is (an index
; of) a closure is evaluation (in the new environment as described in LCE) of
; each body term (in order) followed by producing the value of the result term.

; Evaluation of assignment is evaluation of the update term in the current
; environment, replacement of the variable's value in the current environment,
; producing the void value (which the implementation names and exports as Void).

; · Design and Testing

; Create a good test suite for evo, with a comment above each test case giving
; a clear rationale for its inclusion.

; Also include test cases for any significant helper functions you create
; (e.g. a recursive function is likely to be significant) to aid debugging, and
; export those helper functions from A1.evo.rkt so they can be referenced here.

; Illustrate embedding racket function call, boxed values in environments, and
; result value of assignment.
(check-equal? (evo `((λ (x) (set! x (,add1 x))) 1))
              `(,Void #hash((E1 . ((x #&2) E0))
                            (λ0 . ((λ (x) (set! x (,add1 x))) E0)))))


; ************************* The test cases for the function `evo` ****************************

; A literal value
(check-equal? (evo 234) `(234 #hash()))
(check-equal? (evo add1) `(,add1 #hash()))
(check-equal? (evo -) `(,- #hash()))

; A Racket function call
(check-equal? (evo `(,sub1 100)) `(99 #hash()))
(check-equal? (evo `(,add1 (,add1 (,sub1 100)))) `(101 #hash()))
(check-equal? (evo `(,positive? (,- 1))) `(#f #hash()))

; A multi-layer lambda function call with one body term only.
; It should execute the body and return the result of it.
(check-equal? (evo `(((λ (x) (λ (y) (set! x (,add1 (,add1 y))))) 1) 1000))
              `(,Void #hash(
                            (λ0 . ((λ (x) (λ (y) (set! x (,add1 (,add1 y))))) E0))
                            (E1 . ((x #&1002) E0))
                            (λ1 . ((λ (y) (set! x (,add1 (,add1 y)))) E1))
                            (E2 . ((y #&1000) E1))
                            )))

; A multi-layer lambda function call with multiple body terms and a result term.
; It should execute all bodies and return the result of the last term.
(check-equal? (evo `(((λ (x) (λ (y) (set! y x) (set! x 0) (,add1 y)))100)9))
                   `(101 #hash(
                              (λ0 . ((λ (x) (λ (y) (set! y x) (set! x 0) (,add1 y))) E0))
                              (E1 . ((x #&0) E0))
                              (λ1 . ((λ (y) (set! y x) (set! x 0) (,add1 y)) E1))
                              (E2 . ((y #&100) E1))
                              )))

; A multi-layer lambda function call with the same named parameters x.
; It should update that parameter's most inner scope value
(check-equal? (evo `((((λ (x) (λ (y) (λ (x) (set! x 100) (,- 100))))1)2)3))
               `(-100 #hash(
                              (λ0 . ((λ (x) (λ (y) (λ (x) (set! x 100) (,- 100)))) E0))
                              (E1 . ((x #&1) E0))
                              (λ1 . ((λ (y) (λ (x) (set! x 100) (,- 100))) E1))
                              (E2 . ((y #&2) E1))
                              (λ2 . ((λ (x) (set! x 100) (,- 100)) E2))
                              (E3 . ((x #&100) E2))
                              )))


; ************************* The test cases for the helper function `apply-assignment-stmt` ****************************

; Description of usage:
; (apply-assignment-stmt 'x 100 input-env-clsr-map 'E3) will assign the value 100 to the
; variable x in the input-env-clsr-map while it will search for x in the input-env-clsr-map
; starting from the env E3

(define expected-env-clsr-map (make-hash))
; Given the term is `((((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323))))1)2)3)
(hash-set! expected-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323)))) E0))
(hash-set! expected-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! expected-env-clsr-map 'λ1 `((λ (y) (λ (x) (set! x 100) (,add1 323))) E1))
(hash-set! expected-env-clsr-map 'E2 `((y #&2) E1))
(hash-set! expected-env-clsr-map 'λ2 `((λ (x) (set! x 100) (,add1 323)) E2))
(hash-set! expected-env-clsr-map 'E3 (list (list 'x (box 100)) 'E2))

(define input-env-clsr-map (make-hash))
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323)))) E0))
(hash-set! input-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) (set! x 100) (,add1 323))) E1))
(hash-set! input-env-clsr-map 'E2 `((y #&2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (x) (set! x 100) (,add1 323)) E2))
(hash-set! input-env-clsr-map 'E3 (list (list 'x (box 9999)) 'E2))

; Expect the result of an assignment statement to be void
; Also expect the assignemnt of x = 100 to be applied on the 
; closest scope where x is defined by searching for the binding
; of x starting from the current env E3
(check-equal? (apply-assignment-stmt 'x 100 input-env-clsr-map 'E3) Void)
(check-equal? input-env-clsr-map expected-env-clsr-map)


; ************************* The test cases for the helper function `add-var-to-map` ****************************

; . Test case 1

; Given input-env-clsr-map with an entry for λ0, the function `add-var-to-map` should
; add the given value for its arg variable to the map. Also it sholuld return a list
; containing all the body terms and result term of the λ0.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323))))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323)))) E0))

(hash-clear! expected-env-clsr-map)
(hash-set! expected-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323)))) E0))
(hash-set! expected-env-clsr-map 'E1 `((x #&1) E0))

(check-equal? (add-var-to-map 'λ0 1 input-env-clsr-map 'E1) `((λ (y) (λ (x) (set! x 100) (,add1 323)))))
(check-equal? input-env-clsr-map expected-env-clsr-map)


; . Test case 2

; Given input-env-clsr-map with an entry for λ1, the function `add-var-to-map` should
; add the given value for its arg variable to the map. Also it sholuld return a list
; containing all the body terms and result term of the λ1.

; Fill in the input-env-clsr-map with the entries for the term
; `(((λ (x) (λ (x) (set! x 100) (,add1 323)))1)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (x) (set! x 100) (,add1 323))) E0))
(hash-set! input-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (x) (set! x 100) (,add1 323)) E1))

(hash-clear! expected-env-clsr-map)
(hash-set! expected-env-clsr-map 'λ0 `((λ (x) (λ (x) (set! x 100) (,add1 323))) E0))
(hash-set! expected-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! expected-env-clsr-map 'λ1 `((λ (x) (set! x 100) (,add1 323)) E1))
(hash-set! expected-env-clsr-map 'E2 `((x #&3) E1))

(check-equal? (add-var-to-map 'λ1 3 input-env-clsr-map 'E2) `((set! x 100) (,add1 323)))
(check-equal? input-env-clsr-map expected-env-clsr-map)



; **** Note: The helpers `find-key-for-var` and `find-identifier-value` below are the same functions defined in the
; "eve.rkt" but here I am testing them with the possible terms of the "evo" as well.

(define copied-map (make-hash))

; ************************* The test cases for the helper function `find-identifier-value` ****************************

; . Test case 1
; Given input-env-clsr-map with one key-value entry for x as 'E1 . `((x #&1) E0),
; `find-identifier-value` should return the value 1.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (t) (set! x 100) (,add1 323))))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323)))) E0))
(hash-set! input-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) (set! x 100) (,add1 323))) E1))
(hash-set! input-env-clsr-map 'E2 `((y #&2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (t) (set! x 100) (,add1 323)) E2))
(hash-set! input-env-clsr-map 'E3 `((t #&3) E2))

; Make a copy of input-env-clsr-map to check that the function `find-identifier-value` will not mutate it
(set! copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-identifier-value 'x input-env-clsr-map 'E3) #&1)
(check-equal? input-env-clsr-map copied-map)


; . Test case 2
; Given input-env-clsr-map with two entries for x but `find-identifier-value`
; should return the value related to the most recent scope 'E3 `((x #&3) E2)
; which is 3.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323))))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-clear! copied-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323)))) E0))
(hash-set! input-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) (set! x 100) (,add1 323))) E1))
(hash-set! input-env-clsr-map 'E2 `((y #&2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (x) (set! x 100) (,add1 323)) E2))
(hash-set! input-env-clsr-map 'E3 `((x #&3) E2))

; Make a copy of input-env-clsr-map to check that the function `find-identifier-value` will not mutate it
(set! copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-identifier-value 'x input-env-clsr-map 'E3) #&3)
(check-equal? input-env-clsr-map copied-map)



; ************************* The test cases for the helper function `find-key-for-var` ****************************

; . Test case 1
; Given input-env-clsr-map with one key-value entry for x as 'E1 . `((x #&1) E0),
; `find-key-for-var` should return a list containing the parent env E0 and
; extended new env E1.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (t) (set! x 100) (,add1 323))))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (t) (set! x 100) (,add1 323)))) E0))
(hash-set! input-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (t) (set! x 100) (,add1 323))) E1))
(hash-set! input-env-clsr-map 'E2 `((y #&2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (t) (set! x 100) (,add1 323)) E2))
(hash-set! input-env-clsr-map 'E3 `((t #&3) E2))

; Make a copy of input-env-clsr-map to check that the function `find-key-for-var` will not mutate it
(set! copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-key-for-var 'x input-env-clsr-map 'E3) (list 'E0 'E1))
(check-equal? input-env-clsr-map copied-map)


; . Test case 2
; Given input-env-clsr-map with two entries for x but `find-key-for-var`
; should return a the list of info from the most recent scope 'E3 `((x #&3) E2)
; containing the parent env E2 and extended new env E3.

; Fill in the input-env-clsr-map with the entries for the term
; `((((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323))))1)2)3)
(hash-clear! input-env-clsr-map)
(hash-clear! copied-map)
(hash-set! input-env-clsr-map 'λ0 `((λ (x) (λ (y) (λ (x) (set! x 100) (,add1 323)))) E0))
(hash-set! input-env-clsr-map 'E1 `((x #&1) E0))
(hash-set! input-env-clsr-map 'λ1 `((λ (y) (λ (x) (set! x 100) (,add1 323))) E1))
(hash-set! input-env-clsr-map 'E2 `((y #&2) E1))
(hash-set! input-env-clsr-map 'λ2 `((λ (x) (set! x 100) (,add1 323)) E2))
(hash-set! input-env-clsr-map 'E3 `((x #&3) E2))

; Make a copy of input-env-clsr-map to check that the function `find-key-for-var` will not mutate it
(set! copied-map (hash-copy input-env-clsr-map))

(check-equal? (find-key-for-var 'x input-env-clsr-map 'E3) (list 'E2 'E3))
(check-equal? input-env-clsr-map copied-map)







