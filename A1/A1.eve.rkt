#lang racket ; CSC324 — 2022W — Assignment 1 — Eve Implementation


; Task: implement eve according to A1.eve-test.rkt.

(provide
 (contract-out (eve (any/c . -> . (list/c any/c (hash/c symbol? list?))))
               ; Add any helper functions you tested in A1.eva-test.rkt.
               ; Whether you add contracts to them is optional.
               #;a-helper
               (find-key-for-var (any/c any/c any/c . -> . any/c))
               (find-identifier-value (any/c any/c any/c . -> . any/c))
               (add-var-to-map (any/c any/c any/c any/c . -> . any/c))
               ))

; · Support: indexer

; A constructor for a zero-arity function that when called successively
; produces symbols of the form prefix0 prefix1 prefix2 etc.

(provide (contract-out (indexer (any/c . -> . (-> symbol?)))))

(define (indexer prefix)
  (define last-index -1)
  (λ ()
    (local-require (only-in racket/syntax format-symbol))
    (set! last-index (add1 last-index))
    (format-symbol "~a~a" prefix last-index)))


; . helpers

(define (add-closure lambda-expr env-clsr En λn)
  (let ()
    (hash-set! env-clsr λn (list lambda-expr En))
    λn ; return index of the added closure
    )
  )


(define (find-key-for-var var env-clsr cur-env)
  (let ()
    (if (equal? cur-env 'E0)
        'E0 ; expected to never happen since we do not have open terms in this language
        (let ([item (hash-ref env-clsr cur-env)])
          (let ([parent-env (second item)]
                [v (first (first item))]
                [value (second (first item))]
                )
            (if (equal? var v) ; assumming we eventually find the value of the identifier
                (list parent-env cur-env) 
                (find-key-for-var var env-clsr parent-env)
                )
            )
          )
        )
    )
  )


(define (find-identifier-value identifier env-clsr cur-env)
  (let () ; assume the identifier is defined at least in E0
    (if (equal? cur-env 'E0)
        ;if
        identifier
        ;else
        (let ([item (hash-ref env-clsr cur-env)])
          (let ([nxt-env (second item)]
                [var (first (first item))]
                [value (second (first item))]
                )
            (if (equal? identifier var) ; assumming we eventually find the value of the identifier
                value
                (find-identifier-value identifier env-clsr nxt-env)
                )
            )
          )
        )
    )
  )


(define (add-var-to-map closure-lambda-idex arg-value environments-closures execution-env)
  (let ([closure-fn (first (hash-ref environments-closures closure-lambda-idex))]
        [closure-env (second (hash-ref environments-closures closure-lambda-idex))])
    (let ([lambda-param (first (second closure-fn))]
          [lambda-body (third closure-fn)] ; lambda expression has only one body term which is the third element of the list
          )
      (hash-set! environments-closures execution-env (list (list lambda-param arg-value) closure-env))
      lambda-body 
      )
    )
  )

; · eve

; Produce a two-element list with the value and the environment-closure table
; from evaluating an LCE term.
(define (eve term)

  ; A mutable table of environments and closures.
  ; Task: Look up hash-ref and hash-set! in the racket documentation.
  (define environments-closures (make-hash))
  
  ; Iterators to produce indices for environments and closures.
  (define En (indexer 'E))
  (define λn (indexer 'λ))

  ; Task: complete rec.
  (define (rec t E)
    (let ()
      (match t
        ((list 'λ param body) (add-closure t environments-closures E (λn))) ; returns the index of lambda closure
        ((list fn-expr arg) (let ([fn-expr-eval (rec fn-expr E)] ; a function call
                                  [nxt-execution-env (En)])
                              (rec (add-var-to-map fn-expr-eval (rec arg E) environments-closures nxt-execution-env) nxt-execution-env)
                              )
                            )
        (literal (find-identifier-value literal environments-closures E)) ; base case - literal are values themselves
        )
      )
    )
  
  (list (rec term (En)) 
        (make-immutable-hash (hash->list environments-closures))))

