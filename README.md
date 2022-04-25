# CSC324

## **Principles of Programming Languages**

**Note**: These are the assignments that I completed in this course. That means the skeleton of the project was given to us by the instructor and I worked on completing the required tasks.
In this course, we worked with `Racket` and `Haskell` programming languages to explore programming languages’ concepts with a focus on `functional programming`. I completed multiple exercises and 2 main assignments in this course. Specifically, I worked on creating an interpreter for some non-typed languages. I also worked on building a type system for a typed lanaguage. Both of these languages were based on lambda calculus.

### **`Assignment 1`**

In this assignment, I worked on creating 3 different interpreters for 3 different languages which were extensions of the lambda calculus:

- `LCA` which stands for *Lambda Calculus extension for evA*.

- `LCE` which stands for *Lambda Calculus extension for evE*.

- `LCO` which stands for *Lambda Calculus extension for evO*.

`LCA` and `LCE` both have the same grammar rules (i.e. syntax) as follows:

```
term = (λ (<parameter-identifier>) <body-term>)  ["λ term"]
    | (<function-term> <argument-term>)  ["function call term"]
    | <identifier>  ["variable term"]
    | <literal>  ["literal term"]
```

Semantics of `LCA` is as follows:

```
Assume that the evaluator is only asked to evaluate closed terms that are
semantically valid (satisfy the evaluation assumptions mentioned below).
This ends up guaranteeing that the recursive evaluation described below
only produces values (exercise: prove this by structural induction).

Evaluation of a value is just the value itself.

Evaluation of function call is eager algebraic substitution of argument value.
More precisely, evaluation of (<function-term> <argument-term>) is:
    1. Evaluate <function-term>.
    Evaluation Assumption: the evaluation is not an infinite recursion and the result is a closed λ term 
    (λ (<id>) <body>).
    2. Evaluate <argument-term> to produce a value v. Evaluation Assumption: the evaluation is not an infinite recursion.
    3. Substitute occurrences of variable term <id> in <body> with v. Substitution respects scope: if <body> contains a λ term whose parameter is also <id> it does not replace <id> inside that λ term.
    4. Produce the evaluation of the transformed body.
```

Semantics of `LCE` is as follows:

```
Evaluation now takes both a term and the index of an environment that contains (at least) bindings for the open variables in the term.

Evaluation of a value is still just the value itself.

Evaluation of a variable is its most local value starting from the environment and continuing up the parent chain.

Evaluation Assumption: the variable has a binding in the chain.

Evaluation of function call is still eager and by value, but with argument passed by environment. More precisely, evaluation of (<function-term> <argument-term>) is:
    1. Evaluate <function-term> in the environment.
        Evaluation Assumption: the evaluation is not an infinite recursion and the result is the index of a closure with λ term (λ (<id>) <body>)) and environment E.
    2. Evaluate <argument-term> in the environment to produce a value v. Evaluation Assumption: the evaluation is not an infinite recursion.
    3. Produce the evaluation of <body> in a new environment extending E with   a local binding of <id> to v.

Evaluation of a λ term is the index of a new closure containing the term and environment index.
```


Also, the grammar rules (i.e. syntax) of `LCO` is as follows:

```
term = (λ (<parameter-identifier>) <body-term> ... <result-term>) ["λ term"]
        | (<function-term> <argument-term>)  ["function call term"]
        | (set! <variable-identifier> <update-term>)  ["assignment term"]
        | <identifier>  ["variable term"]
        | <literal>  ["literal term"]
```

Semantics of `LCO`:

```
Evaluation still takes a term and an index of an appropriate environment.

Evaluation of a value or λ is as for LCE.

Evaluation of a variable is as for LCE (with the extra step of extracting its current value from the box it is stored in).

Evaluation of function call allows the value of the function term to be a racket procedure, in which case the evaluation is the result of calling that procedure on the value of the argument term.

Evaluation of function call when the value of the function term is (an index of) a closure is evaluation (in the new environment as described in LCE) of each body term (in order) followed by producing the value of the result term.

Evaluation of assignment is evaluation of the update term in the current environment, replacement of the variable's value in the current environment, producing the void value (which the implementation names and exports as Void).
```

I created 3 different interpreters for these 3 lanaguegs as follows:

`Eva` is an `Eager By-Value Algebraic` interpreter for the language `LCA`.

`Eve` is an `Eager By-Value Environmental` interpreter for the language `LCA`.

`Evo` is an `Eager By-Value Stateful` interpreter for the language `LCA`.

**Examples of how `EVA` evaluates `LCA` terms**

```
A 3-layer lambda expression with one call on the parameter x in different scopes

(check-equal? (eva '((λ (x) (λ (t) (λ (y) x))) 1)) '(λ (t) (λ (y) 1)))
(check-equal? (eva '((λ (x) (λ (t) (λ (y) x))) ((λ (x) 1) 2))) '(λ (t) (λ (y) 1)))
(check-equal? (eva '((λ (x) (λ (x) (λ (y) x))) 20)) '(λ (x) (λ (y) x)))
(check-equal? (eva '((λ (x) (λ (y) (λ (y) x))) 20)) '(λ (y) (λ (y) 20)))
```

Note that `check-equal?` belongs to a `Racket` library for testing.

**Examples of how `EVE` evaluates `LCE` terms**

```
(check-equal? (eve '(((λ (x) (λ (y) 1)) 20) 300))
              '(1 #hash((E1 . ((x 20) E0))
                        (E2 . ((y 300) E1))
                        (λ0 . ((λ (x) (λ (y) 1)) E0))
                        (λ1 . ((λ (y) 1) E1)))))
```

**Examples of how `EVO` evaluates `LCO` terms**

```
(check-equal? (evo `((λ (x) (set! x (,add1 x))) 1))
              `(,Void #hash((E1 . ((x #&2) E0))
                            (λ0 . ((λ (x) (set! x (,add1 x))) E0)))))
```

You can view the implementation for `EVA`, `EVE` and `EVO` in [*A1.eva.rkt*](/A1/A1.eva.rkt), [*A1.eve.rkt*](/A1/A1.eve.rkt) and [*A1.evo.rkt*](/A1/A1.evo.rkt), respectively. 

### **`Assignment 2`**

In this assignment I created a `type inference system` which discovers the expression types by solving the `Unification` problem. We know in static typed languages, specifically in some languages like `Haskell` which specifying the types is optional, the compiler should be able to infer some/all of the types of expressions by using a sophisticated type system. In this assignment, I created a `type inference system` for a **typed** language based on lambda calculus called `LCT`.

The `LCT` language has the following grammar and syntax:

```
An LCT expression is one of:
    (λ (<parameter-identifier>) <body-expression>)  ["λ expression"]
    (<function-expression> <argument-expression>)   ["function call expression"]
    <identifier>                                    ["variable expression"]
    <literal>                                       ["literal expression"]
```

We will assume that each parameter has a unique name, and that identifiers do not begin with the character `α`.

The **type** of the expressions in the `LCT` (i.e. the grammar for the type language of `LCT`) are as follows:

```
An LCT type is one of:
    (-> <argument-type> <result-type>) ["function type"]
    Boolean                            ["boolean type"]
    Number                             ["number type"]
    <type-variable>                    ["type variable"]
```

To read more about how to determine the type of the expressions in the `LCT`, refer to [*A2.infer-test.rkt*](/A2/A2.infer-test.rkt).


When our type system wants to determine the type of an expression **e**, it goes through the following process: 

- Determine a general preliminary type for the expression **e**
- Find out all of the type constraints of the *expression* through recursion
- Solve the `Unification` problem when the type constraints are the equations that need to be unified. The `Unification` problem returns a function `σ` which maps every term of the **e** to a specific type in the language. So we can find the concrete and final type of the **e** by calculating `σ(e)`.

For example, given the following `LCT`'s expression:

```
(λ (x) ((+ x) 5))
```

Its preliminary type is `(-> αx α3)`, while its list of type constraints is `((α2 (-> Number α3)) (α+ (-> αx α2)))`. Now we consider each of the sublists of the type constraint list as two equations that we hope we will be able to unify. So given the list `(t1 t2)` we say the equation `t1 ≡ t2` is the claim that there exists a substitution `σ` so that `σ(t1) = σ(t2)`. If the claim is true, such a substitution is called a `unifier` of `t1` and `t2`. So the terms `t1` and `t2` are equivalent (i.e. `t1 ≡ t2`), if there exists a substitution (i.e. the unifier) `σ` so that `σ(t1) == σ(t2)`. We call this substitution a `unifier` of `t1` and `t2`, since this unifier proves that these two terms are equivalent in terms of type. For example if `t1` is mapped to have the type `(-> Number Number)` and `t2` also is mapped to have the type `(-> Number Number)`, then we say `t1 ≡ t2`. 

For finding the substitution `σ`, we need to solve the `Unification` problem by following its algorithm. The `Unification` problem takes a list of equations and tries to find a substitution that unifies each equation in the most general way possible. Such a substitution (if it exists) is known as the `most general unifier` (mgu) of the equations.

**Unification Algorithm**

```
For an empty list of equations the mgu is the identity function.

Otherwise we have a list of equations t ≡ t', t1 ≡ t1', t2 ≡ t2', etc.

A necessary (but not sufficient) for an mgu to exist is then: t and t′ are both the same constant, or both are compound terms with the same functor and number of argument terms, or at least one is a variable.

Suppose t and t' are equal. The mgu is then the mgu (if it exists) of the other equations: t1 ≡ t1', t2 ≡ t2', etc.

Suppose t and t' are compound terms (f a1 ...) and (f a1' ...) with the same number of arguments. The mgu is then the mgu (if it exists) of the equations where t ≡ t' is replaced with equations between all the argument terms:  a1 ≡ a1', ...,  t1 ≡ t1', t2 ≡ t2', etc.

Suppose t is a variable (and t' isn't t). Let σ be the substitution that maps t to t'. The mgu then exists iff t doesn't occur in t', and the equations σ(t1) ≡ σ(t1'), σ(t2) ≡ σ(t2'), etc have an mgu σ′. The mgu is then σ' ∘ σ.

Suppose none of the the previous apply and t' is a variable. The mgu is then the mgu (if it exists) of the equations with t and t′ swapped in the first equation t' ≡ t, t1 ≡ t1', t2 ≡ t2', etc.
```
For more about the `Unification` problem refer to [*A2.unify-test.rkt*](/A2/A2.unify-test.rkt). The implementation for the unifier algorithm is in [*A2.unify.rkt*](/A2/A2.unify.rkt).


So now given the expression exmaple  

```
(λ (x) ((+ x) 5))
```

Our type system can determine the final type of it in two different situations:

1) Without any extra environment type constraint, the type system cannot not infer any further type other than the initial preliminary type. So the final type of our example expression will determined to be:

```
(-> αx α3)
```

2) With the environment type constraint for + as `((α+ (-> Number (-> Number Number)))`, the type system can infer a more concrete final type for our example expression as:

```
(-> Number Number)
```

The implementation for the type inference function `infer` is done in [*A2.test.rkt*](/A2/A2.test.rkt).

We can see that the type system powerfully use the `Unification` algorithm and the extra environment type constraints to come up with a definite final type for any expression. Such powerful type systems are the basis of the static typed languages like `Haskell`.

**Running the Racket files in both assignments**

To run the Racket files of the assignments, you can download the `DrRacket` IDE.

## **Exercise 8**

In this exercise, we practiced working with `Maybe` type and stateful computations in `Haskell`. To do that, we implemented a mini-interpreter in Haskell for a simple expression-based imperative programming language. 

This simple programming language has the following grammar rules:

```
data Expr = Plus Expr Expr     
          | Minus Expr Expr    
          | Var String          
          | Lit Int             
          | Assign String Expr  
          | BlockExprs [Expr]   
          | Loop Expr         
```

To see the complete description and implementation of this exercise, refer to [*exercise-8.hs*](/exercise8/exercise-8.hs).

**Note**

As this is a public repo, I have added the course assignments here and most of the completed exercises are added to my private repo. Please contact me to view them as well. Thank you.
