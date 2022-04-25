#lang racket

; CSC324 — 2022W — Assignment 2 — Type Constraints Implementation

(provide (contract-out (type-and-constraints
                        (-> any/c any/c
                            (list/c any/c (listof (list/c any/c any/c))))))
         α?
         α-type-generator)

; · Support

; Whether t represents a type variable.
(define (α? t)
  (and (symbol? t) (equal? #\α (string-ref (symbol->string t) 0))))

; Generator generator for type variables.
#;((α-type-generator)) ; type variable of the form αn
#;((α-type-generator) id) ; type variable of the form αid
(define (α-type-generator)
  (let ([index -1])
    (λ ((id #f))
      (set! index (add1 index))
      (local-require (only-in racket/syntax format-symbol))
      (format-symbol "α~a" (if id id index)))))


; · type-and-constraints

; A type and list of constraints for expression e, using αs to generate
; type variables.
(define (type-and-constraints e αs) ; returns pattern (list 'α0 (list))
  (match e
    [(list 'λ (list param) body) (let ([body-type (type-and-constraints body αs)])
                                   (list (list '-> (αs param) (first body-type)) (second body-type))
                                   )]
    [(list fn arg) (let ([fn-expr-type (type-and-constraints fn αs)]
                         [arg-type (type-and-constraints arg αs)]
                         [preliminary-type (αs)])
                     (list preliminary-type ; its own preliminary type αn
                           (append
                            (list (list (first fn-expr-type) (list '-> (first arg-type) preliminary-type))) ; constraints of its function expression's type and its type as (-> argument-type αn)
                            (append (second fn-expr-type) (second arg-type))) ; constraints of its function and arg expressions     
                           ) 
                     )
                   ]
    [#t (list 'Bool '())]
    [#f (list 'Bool '())]
    [anything (let ()
                (if (number? anything)
                    (list 'Number '())
                    (list (αs anything) '())
                    )
                )]
    )
  )
