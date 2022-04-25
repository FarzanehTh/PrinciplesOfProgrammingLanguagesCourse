#lang racket ; CSC324 — 2022W — Assignment 1 — Evo Implementation

; Task: implement evo according to A1.evo-test.rkt.

(require  "A1.eve.rkt")

(provide
 (contract-out (evo (any/c . -> . (list/c any/c (hash/c symbol? list?))))
               ; Add any helper functions you tested in A1.evo-test.rkt.
               ; Whether you add contracts to them is optional.
               #;a-helper
               (apply-assignment-stmt (any/c any/c any/c any/c . -> . any/c))
               (find-key-for-var (any/c any/c any/c . -> . any/c))
               (find-identifier-value (any/c any/c any/c . -> . any/c))
               (add-var-to-map (any/c any/c any/c any/c . -> . any/c))
               ))

; · Support: indexer and Void

(provide (contract-out (indexer (any/c . -> . (-> symbol?)))))

(define (indexer prefix)
  (define last-index -1)
  (λ ()
    (local-require (only-in racket/syntax format-symbol))
    (set! last-index (add1 last-index))
    (format-symbol "~a~a" prefix last-index)))

; There is no literal nor variable for the void value (although it has a printed
; representation when printed inside compound values), so we'll name it here and
; and also export it for testing.

(provide (contract-out (Void void?)))

(define Void (void))


; . helpers

(define (add-closure lambda-expr env-clsr En λn)
  (let ()
    (hash-set! env-clsr λn (list lambda-expr En))
    λn ; return index of the added closure
    )
  )


(define (apply-assignment-stmt var assignment-stmt env-clsr cur-env)
  (let (
        ;[past-env (first (find-key-for-var var env-clsr cur-env))]
        [extended-env (second (find-key-for-var var env-clsr cur-env))]
        )
    (let ([old-value (second (first (hash-ref env-clsr extended-env)))])
      (set-box! old-value assignment-stmt)
      )
    Void
    )
  )

; We also have a similar named helper in "eve", but this one here is different and considers the fact that
; the lambda can have multiple bodies and a result term and hence it returns a list of the lambda's bodies
; and result term
(define (add-var-to-map closure-lambda-idex arg-value environments-closures execution-env)
  (let ([closure-fn (first (hash-ref environments-closures closure-lambda-idex))]
        [closure-env (second (hash-ref environments-closures closure-lambda-idex))])
    (let ([lambda-param (first (second closure-fn))]
          [lambda-body (rest (rest closure-fn))] ; retun all body terms and the result term of the lambda in a list
          )
      (hash-set! environments-closures execution-env (list (list lambda-param (box arg-value)) closure-env))
      lambda-body 
      )
    )
  )

; · evo

; Produce a two-element list with the value and the environment-closure table
; from evaluating an LCO term.
(define (evo term)

  ; A mutable table of environments and closures.
  (define environments-closures (make-hash))
  
  ; Iterators to produce indices for environments and closures.
  (define En (indexer 'E))
  (define λn (indexer 'λ))

  (define (is-lambda? fn-expr)
    (if (symbol? fn-expr)
        #t
        #f
        )
    )
  
  ; Task: complete rec. 
  (define (rec t E)
    (let ()
      (match t
        ((list v) (rec v E))
        ((list 'λ param ... body) (add-closure t environments-closures E (λn))) ; returns the index of lambda
        ((list 'set! var assignment-stmt) (apply-assignment-stmt var (rec assignment-stmt E) environments-closures E)) ; Delay the evaluation of the value term by putting it in a λ
        ((list fn-expr arg ...) (let ([fn-expr-eval (rec fn-expr E)]
                                      [nxt-execution-env (En)]
                                      ) ; Evaluate the fn-expr and then decide based on whether it is a Racket function or a lambda call
                                  (if (is-lambda? fn-expr-eval)
                                      ;if
                                      (rec (add-var-to-map fn-expr-eval (rec arg E) environments-closures nxt-execution-env) nxt-execution-env)
                                      ;else
                                      (if (procedure? fn-expr-eval)
                                          ;if it is a Racket
                                          (fn-expr-eval (rec arg E))
                                          ;else 
                                          (rec arg E) ; arg is a list of body expressions of a lambda body, so evaluate them one by one
                                          )
                                      )
                                  )
                                )
        
        (literal (let ([e (find-identifier-value literal environments-closures E)])
                   (if (box? e)
                       (unbox e)
                       e
                       )
                   )
                 ) ; base case - literal are values themselves
        )
      )
    )

  
  (list (rec term (En))
        (make-immutable-hash (hash->list environments-closures))))

