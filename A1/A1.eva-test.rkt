#lang racket ; CSC324 — 2022W — Assignment 1 - Eva Design and Testing

; • Eva: Eager By-Value Algebraic Evaluator for an Extended Lambda Calculus

; Task: understand the syntax and semantics of the language LCA described below,
; then create a good test suite for an evaluator of LCA named eva which you will
; then implement in A1.eva.rkt

(require "A1.eva.rkt"
         rackunit)

; · Syntax of LCA

; The grammar of “terms” [along with how we'll refer to them] in LCA is:
;   term = (λ (<parameter-identifier>) <body-term>)  [“λ term”]
;        | (<function-term> <argument-term>)  [“function call term”]
;        | <identifier>  [“variable term”]
;        | <literal>  [“literal term”]

; A term where each variable term <id> occurs inside some enclosing λ term
; whose parameter is <id> is called “closed”, otherwise it's called “open”.
; In particular, identifier terms are open and literal terms are closed.

; We will refer to literal terms and closed λ terms (closures) as “values”.

; Parenthesized terms are represented by lists.
; Identifiers (including "λ") are represented by symbols.
; Literals are, and represented by, values that are not lists nor symbols.

; For example, the following represents a (syntactically) valid term in LCA,
; and contains one instance of each production:
#;'(324 (λ (y) y))
; It contains two values:
#;(λ (y) y)
#;324

; · Semantics of LCA

; Assume that the evaluator is only asked to evaluate closed terms that are
; semantically valid (satisfy the evaluation assumptions mentioned below).
; This ends up guaranteeing that the recursive evaluation described below
; only produces values (exercise: prove this by structural induction).

; Evaluation of a value is just the value itself.

; Evaluation of function call is eager algebraic substitution of argument value.
; More precisely, evaluation of (<function-term> <argument-term>) is:
;   1. Evaluate <function-term>.
;      Evaluation Assumption: the evaluation is not an infinite recursion
;      and the result is a closed λ term (λ (<id>) <body>).
;   2. Evaluate <argument-term> to produce a value v.
;      Evaluation Assumption: the evaluation is not an infinite recursion.
;   3. Substitute occurrences of variable term <id> in <body> with v.
;      Substitution respects scope: if <body> contains a λ term whose parameter
;      is also <id> it does not replace <id> inside that λ term.
;   4. Produce the evaluation of the transformed body.

; · Design and Testing

; Create a good test suite for eva, with a comment above each test case giving
; a clear rationale for its inclusion.

; Also include test cases for any significant helper functions you create
; (e.g. a recursive function is likely to be significant) to aid debugging, and
; export those helper functions from A1.eva.rkt so they can be referenced here.


; ************************* The test cases for the function `eva` ****************************

; A literal term.
(check-equal? (eva 324) 324)

; A lambda expression term
(check-equal? (eva '(λ (x) x)) '(λ (x) x))

; A one layer function call
(check-equal? (eva '((λ (x) x) 2)) 2)

; A lambda expression whose body is a function call
(check-equal? (eva '((λ (x) ((λ (y) x)1))100)) 100)
(check-equal? (eva '((λ (x) ((λ (y) y)1))100)) 1)
(check-equal? (eva '((λ (x) ((λ (y) ((λ (t) t) -1))1))100)) -1)
(check-equal? (eva '((λ (x) ((λ (y) x)-1)) 1000)) 1000)

; A 3-layer lambda expression with one call on the parameter x in different scopes
(check-equal? (eva '((λ (x) (λ (t) (λ (y) x))) 1)) '(λ (t) (λ (y) 1)))
(check-equal? (eva '((λ (x) (λ (t) (λ (y) x))) ((λ (x) 1) 2))) '(λ (t) (λ (y) 1)))
(check-equal? (eva '((λ (x) (λ (x) (λ (y) x))) 20)) '(λ (x) (λ (y) x)))
(check-equal? (eva '((λ (x) (λ (y) (λ (y) x))) 20)) '(λ (y) (λ (y) 20)))

; A 3-layer lambda expression with two calls tested with two different approches
(check-equal? (eva (list (eva '((λ (x) (λ (t) (λ (y) x))) 1)) 2)) '(λ (y) 1))
(check-equal? (eva '(((λ (x) (λ (x) (λ (y) x)))1)2)) '(λ (y) 2))

; A 3-layer lambda expression with three calls tested with two different approches
(check-equal? (eva (list (eva (list (eva '(( λ(x) (λ (t) (λ (y) x))) 1)) 2)) 3)) 1)
(check-equal? (eva '((((λ(x) (λ (t) (λ (y) x))) 1)2)3)) 1)

; A 4-layer lambda expression with four calls tested with two different approches
(check-equal? (eva (list (eva (list (eva (list (eva '((λ (x) (λ (t) (λ (x) (λ (z) x)))) 1)) 2)) 3)) 4 )) 3)
(check-equal? (eva '(((((λ (x) (λ (x) (λ (x) (λ (x) x)))) 1)2)3)4)) 4)


; *********************** The test cases for the helper function `apply-value` *************************

; The function `apply-value` is called like (apply-value body param value) and it applies the value
; of the param on the body of a function. This function is called by the helper `call-fn`.

; A literal value 
(check-equal? (apply-value 1 'x 2) 1)

; A non-list body
(check-equal? (apply-value 'x 'x 2) 2)

; A function call on a body such that the varibale x does not has a more local scope, should apply the given value to the body
(check-equal? (apply-value '(λ (y) (λ (t) x)) 'x 2) '(λ (y) (λ (t) 2)))
(check-equal? (apply-value '(λ (y) (λ (y) (λ (t) x))) 'x 2) '(λ (y) (λ (y) (λ (t) 2))))

; A function call on a body such that the varibale x has a more local scope, should not apply the given value to the body
(check-equal? (apply-value '(λ (x) (λ (x) x)) 'x 2) '(λ (x) (λ (x) x)))
(check-equal? (apply-value '(λ (x) (λ (t) (λ (y) x))) 'x 2) '(λ (x) (λ (t) (λ (y) x))))


; ************************* The test cases for the function `map-rec` ****************************

; `map-rec` is called by `apply-value` and so it is never called if the body of the given function contains an element
; like (param). So give (map-rec body param value), the function `map-rec` applies the value of param in the body.

; An empty list 
(check-equal? (map-rec '() 'x 2) '())

; Lambda expressions such that variable x has occured in the body and should be substituted
(check-equal? (map-rec '(λ (y) x) 'x 2) '(λ (y) 2))
(check-equal? (map-rec '(λ (t) (λ (y) x)) 'x 1) '(λ (t) (λ (y) 1)))


; A lambda expression such that variable x has not occured in the body and should not be substituted 
(check-equal? (map-rec '(λ (t) (λ (y) u)) 'x 1) '(λ (t) (λ (y) u)))


