#lang racket

; CSC324 — 2022W — Assignment 2 — Unifier Implementation

(provide (contract-out (mgu (-> (-> any/c boolean?)
                                (listof (list/c any/c any/c))
                                (or/c #f (-> any/c any/c))))))

; support functions

(define (have-necessary-conditions variable? equation)
  (let ([f (first equation)]
        [s (second equation)])
    (cond [(and (number? f) (number? s)) #t]
          [(and (boolean? f) (boolean? s)) #t]
          [(or (variable? f) (variable? s)) #t]
          [(and (equal? (first f) (first s)) (equal? (length (rest f)) (length (rest s)))) #t]
          [else #f]
          )
    )
  )

(define (check-all-equations variable? equation)
  (if (empty? equation)
      #t
      (and (have-necessary-conditions variable? (first equation)) (check-all-equations variable? (rest equation)))
      )
  )

(define (occurred v t)
  (if (list? t)
      (if (empty? t)
          #f
          (or (occurred v (first t)) (occurred v (rest t)))
          )
      (equal? v t)
      )
  )

(define (get-new-equations lst1 lst2)
  (if (empty? lst1)
      '()
      (let ()
        (append (list (list (first lst1) (first lst2))) (get-new-equations (rest lst1) (rest lst2)))
        )
      )
  )

(define (replace new-assignment e)
  (if (list? e)
      (if (empty? e)
          '()
          (cons (replace new-assignment (first e)) (replace new-assignment (rest e)))
          )
      (if (equal? e (first new-assignment))
          (second new-assignment)
          e
          )
      )
  )

(define (update-equations new-assignment eqs)
  (if (empty? eqs)
      '()
      (cons (replace new-assignment (first eqs)) (update-equations new-assignment (rest eqs)))
      )
  )

(define (find map key)
  (if (empty? map)
      key ; if not found, key maps to itself
      (let ([e (first map)])
        (if (equal? key (first e))
            (second e)
            (find (rest map) key)
            )
        )
      )
  )

(define (find-unification map e)
  (if (list? e)
      (if (empty? e)
          '()
          (let ([result (find map (first e))])
            (if (list? result)
                (cons (find-unification map result) (find-unification map (rest e)))
                (cons result (find-unification map (rest e)))
                )
            )
          )
      (let ([result (find map e)])
        (if (list? result)
            (find-unification map result)
            result
            )
        )
      )
  )

; The mgu of equations, or #f if there is no mgu, where the unary predicate
; variable? identifies variables.
(define (mgu variable? equations)

  ; map of unifications
  (define map '())
  
  (define (rec eqs)
    (if (empty? eqs)
        '()
        (let ([first-eq (first eqs)]
              [other-eqs (rest eqs)])
          (match first-eq
            [(list (list f1 args1 ...) (list f2 args2 ...)) (let ()
                                                              (rec (append (get-new-equations args1 args2) other-eqs))
                                                              )]
            [(list f s) (cond [(equal? f s) (let ()
                                              (set! map (cons (list f s) map))
                                              (rec other-eqs)    
                                              )]
                              [(and (variable? f) (not (equal? f s))) (if (occurred f s) ; if var f occurs inside the term s, we cannot find a unifier for these equations.
                                                                          #f
                                                                          (let ()
                                                                            (set! map (cons (list f s) map))
                                                                            (rec (update-equations (list f s) other-eqs))
                                                                            ))
                                                                      ]
                              [(variable? s) (if (occurred s f) ; if var s occurs inside the term f, we cannot find a unifier for these equations.
                                                 #f
                                                   (rec (cons (list s f) other-eqs))
                                                 )
                                             ]
                              )]
            )
          )
        )
    )

  ; check necessary conditions
  (if (not (check-all-equations variable? equations))
      ; if
      #f
      ; else
      (let ()
        (rec equations) ; try to unify the equations
        (println map)
        (λ (e) (find-unification map e)) ; return σ
        )
      )
  )

