#lang racket ; CSC324 — 2022W — Assignment 1 — Eva Implementation

; Task: implement eva according to A1.eva-test.rkt.

(provide (contract-out
          (eva (any/c . -> . any/c))
          ; Add any helper functions you tested in A1.eva-test.rkt.
          ; Whether you add contracts to them is optional.
          #;a-helper
          (apply-value (any/c any/c any/c . -> . any/c))
          (map-rec (any/c any/c any/c . -> . any/c))
          )
         )


(define (get-parameter fn-expr)
  (if (list? fn-expr)
      (if (list? (second fn-expr))
          (first (second fn-expr))
          '()
          )
      fn-expr
      )
  )


(define (get-body fn-expr)
  (if (list? fn-expr)
      (third fn-expr)
      fn-expr
      )
  )

(define (map-fn e param value)
  (if (equal? e param)
      value
      e))


(define (map-rec body param value)
  (cond [(empty? body) '()]
        [(and (list? body) (equal? (length body) 3) (equal? (second body) (list param))) body] ; this is a function call term, you do not need to go inside
        [(not (list? (first body))) (cons (map-fn (first body) param value) (map-rec (rest body) param value))]
        [else (cons (map-rec (first body) param value) (map-rec (rest body) param value))])
  )

; Substitute the occurances of param in body if a closer scoped param with the same name does not exist
(define (apply-value body param value)
  (if (list? body)
      (let ([result (filter (λ (e) (equal? e (list param))) body)])
        (if (empty? result)
            (map-rec body param value)
            body
            )
        )
      (if (equal? body param)
          value
          body
          )
      )
  ) 

(define (call-fn fn-expr value)
  (let ([param (get-parameter fn-expr)]
        [body (get-body fn-expr)])
    (apply-value body param value)
    )
  )

; Produce the value of a closed term from LCA.
(define (eva term)
  (match term
    ((list 'λ param body) (list 'λ param body)) ; base case  -  a lambda expression is a value itself
    ((list fn-expr arg) (eva (call-fn (eva fn-expr) (eva arg)))) ; call the fn-expr only one time but replace all of occurances of arg value all the way down any where applicable
    (anything anything) ; base case - other literal are values themselves
    )
  )







;;;;;;;;;;;;;;; The below are some extra functions that I used in ealier versions of this assignments that I ended up
; not using. But I am keeping them here just in case.

; Apply the "outer-parameter" with the value "value" to the function "fn". Also keep track of the recursion depth. 
#;(define (apply-fn fn outer-param value rec-depth)
  (cond [(not (list? fn)) fn]
        [else (let ([param (get-parameter fn)]
                    [body (get-body fn)])
                (if (list? body)
                    (if (equal? (length body) 2)  ; body is a function call
                        ;if
                        (let ([evaluated-body (apply-fn (first body) (get-parameter (first body)) (second body) 0)])
                          (if (equal? evaluated-body outer-param)
                              value
                              evaluated-body
                              )
                          )
                        ;else body is a lambda expression
                        (let  ([inner-param (get-parameter body)]
                               [inner-body (get-body body)]
                               )
                          (if (and (list? inner-body) (not (equal? inner-param outer-param)))
                              ;if
                              (if (equal? rec-depth 0)
                                  ;if
                                  (apply-fn body outer-param value (+ 1 rec-depth))
                                  ;else
                                  (list 'λ (list param) (apply-fn body outer-param value (+ 1 rec-depth)))
                                  )
                              ;else - inner-body is not a list so now we can substitite outer-param
                              (if (equal? inner-param outer-param)
                                  ;if
                                  (if (equal? rec-depth 0)
                                      body
                                      (list 'λ (list param) body)
                                      )
                                  ;else
                                  (if (equal? rec-depth 0)
                                      (substitute body outer-param value)
                                      (list 'λ (list param) (substitute body outer-param value))
                                      )
                                  )
                              )
                          )
                        )
                    ;else the fn is just a one layer lambda expression
                    (if (equal? param body)
                        ;if
                        (if (equal? rec-depth 0)
                            value
                            (list 'λ (list param) value)
                            )
                        ;else
                        (if (equal? rec-depth 0) ; body is a literal like a number or string or ...
                            body
                            (list 'λ (list param) body)
                            )
                        )
                    )
                )
              ]
        )
  )



#;(define (replace-if-equal el param-name value)
  (if (equal? el param-name)
      value
      el
      )
  )


#;(define (substitute expr param value)
  (match expr
    ((list 'λ p lambda-expr) (map (λ (e) (replace-if-equal e param value)) expr))
    )
  )


