#lang racket

; CSC324 — 2022W — Assignment 2 — Type Constraints Design and Testing

; • Type and Constraint Generator

; Task: explore the language LCT which has expressions and types, then implement
; a function in A2.infer.rkt to generate a preliminary type and type constraints
; for an expression.

(require "A2.infer.rkt" rackunit)


; · LCT Expressions and Types

; An LCT expression is one of:
;
;   (λ (<parameter-identifier>) <body-expression>)  [“λ expression”]
;   (<function-expression> <argument-expression>)   [“function call expression”]
;   <identifier>                                    [“variable expression”]
;   <literal>                                       [“literal expression”]

; Representation of LCT is as in A1, but literals are only booleans or numbers.

; We will assume that each parameter has a unique name, and that identifiers
; do not begin with the character “α”.

; An LCT type is one of:
;
;   (-> <argument-type> <result-type>) [“function type”]
;   Boolean                            [“boolean type”]
;   Number                             [“number type”]
;   <type-variable>                    [“type variable”]

; The representation of a function type is a three-element list where the first
; element is the symbol “->”.
; The representation of Number and Boolean are the corresponding symbols.
; The representation of a type variable is a symbol starting with “α”.

; An LCT type will also be considered a term in the sense of unification,
; where type variables will be considered term variables.

; · Preliminary Typing of an LCT Expression

; To infer the type of an LCT expression first associate a preliminary type
; and a list of constraints to each sub-expression, by structural recursion.
; The list of constraints is a list of equations (in the sense of unification)
; between LCT types.

; The preliminary type of a boolean or number is Boolean or Number, with an
; empty list of constraints.

; The preliminary type of a variable with name id is the type variable αid,
; with an empty list of constraints.

; The preliminary type of a λ expression is (-> αid body-type), where body-type
; is the preliminary type of the body expression. It has the same constraints
; as the body expression.

; The preliminary type of a function call expression is a new type variable αn
; for some natural number n. The constraints are the combination of:
;   - constraints of its function expression
;   - constraints of its argument expression
;   - a constraint equating the function expression's type and the type
;     (-> argument-type αn) where argument-type is the preliminary type
;     of the argument expression.

; For example, for ...
#;(λ (x) ((+ x) 5)) ; ... the preliminary type could be ...
#;(-> αx α3) ; ... with gathered constraints ...
#;((α2 (-> Number α3))
   (α+ (-> αx α2)))

; Your code might generate a different α numbering, and the constraints in a
; different order.
(check-equal? (type-and-constraints '(λ (x) ((+ x) 5))
                                      (α-type-generator))
                '((-> αx α3)
                  ((α2 (-> Number α3)) (α+ (-> αx α2)))))

; If the constraints for an expression e can be unified by mgu σ, then the type
; of e is σ(e).

; For the above example the constraints are too weak to say more than that the
; expression is (-> αx α3), i.e. some unary function between arbitrarty types.
; But if we added the constraint (α+ (-> Number (-> Number Number)) for the
; open variable + we could determine that the example expression has type
; (-> Number Number). See A2.test.rkt.

