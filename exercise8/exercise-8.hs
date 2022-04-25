-- In this exercise, we are going to practice Maybe type and stateful computation.
-- We will implement a mini-interpreter in Haskell for a simple expression-based
-- imperative programming language. To simplify the task, we suppose the parser
-- part is already done and we work with the AST tree of the language.

-- contains (Node t1 v0 t2) v
--   | v == v0 = True
--   | v < v0 = (contains t1 v) -- check the left sub-tree
--   | otherwise = (contains t2 v)  -- check the right sub-tree

-- First we are going to include all useful definitions about stateful computation.
-- These definitions are the same as we learned in the lecture.
get :: s -> (s, s)
get s = (s, s)

put :: s -> s -> ((), s)
put r _ = ((), r)

pureValue :: a -> s -> (a, s)
pureValue r s = (r, s)

andThen :: (s -> (a, s)) -> (a -> s -> (b, s)) -> (s -> (b, s))
andThen state_based result_state_based s0 =
  let (a0, s1) = state_based s0
  in result_state_based a0 s1

-- Now we define the AST tree of the parsed program. In this simple programming languages,
-- everything is expression (including assignment and loops). The language has binary operators
-- like plus and minus. It has variable reference and assignments. It also has loop expression
-- to execute/evaluate expressions iteratively. All values in this language are integers. We will 
-- explain the semantics of each kind of expressions in comments.
data Expr = Plus Expr Expr      -- Evaluates two subexpressions in order from left to right and then return the addition of the two
          | Minus Expr Expr     -- Evaluates two sub-exprs in order and return the substraction results of the two
          | Var String          -- Return the value of the variable. If the variable is not defined yet, the execution terminates with error.
          | Lit Int             -- An integer constant literal.
          | Assign String Expr  -- Evaluate the sub-expresison, binds the variable denoted by the string to value of the sub-expression. Return the sub-expression value as its own result. 
          | BlockExprs [Expr]   -- Evaluate a sequence of expressions in order. Return the last evaluated expression result and its own result. If the list is empty, return zero.
          | Loop Expr           -- Iteratively executing/evaluating the sub-expression until the sub-expression evaluates to zero. This loop expression always return zero as its result.

-- To implement the interpreter for this language, we are going to maintain a state that tracks
-- the execution environment of the program. The environment maintains the mapping between variable
-- names and its integer values. To keep things simple, we implement this environemnt as a list
-- of pairs. Everytime we add a new variable binding (or update the varaible binding), we will
-- append at the head of the list. When we search the variable binding, we will locate the first
-- matching entry in the list and return the corresponding value. For simplicity, we do not clean
-- this list in our implementation.
type Environment = [(String, Int)]
-- bindVar append additional binding into the head of the environment list.
bindVar :: Environment -> String -> Int -> Environment
bindVar env var val = (var, val):env
-- locate return the matching value. The return type is Maybe Int and we use
-- Nothing to denote the case where we did not find any value 
-- (i.e., variable undefined error).
--
-- Please finish the implementation of this function
locate :: Environment -> String -> Maybe Int
locate [] var = Nothing
locate ((x, val):xs) var 
    |(x == var) = (Just val)
    | otherwise = locate xs var

-- We talked about lift function in the lecture. It works for unary function.
-- Here we want to define a lift function for binary functions. It should return
-- Nothing if either operand is Nothing. This utility function will be used later
-- when we implement the interpreter.
--
-- Please finish the implementation of this function.
binLift :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
binLift op Nothing _ = Nothing
binLift op _ Nothing = Nothing
binLift op (Just x) (Just y) = (Just (op x y)) 

handleAssignment :: String -> Maybe Int -> Environment -> (Maybe Int, Environment)
handleAssignment var Nothing s = 
    andThen (put s) (\_ -> pureValue Nothing) s
handleAssignment var (Just value) s = 
    andThen (put (bindVar s var value)) (\_ -> pureValue (Just value)) s

isResultZero :: Maybe Int -> Bool
isResultZero (Just 0) = True
isResultZero _ = False

-- Our main evaluate function implements the interpretation task. It has the standard
-- stateful computation type signature. It evaluates an expression and returns the integer
-- value as the evaluation result. The mutable state here is of course the environment.
evaluate :: Expr -> Environment -> (Maybe Int, Environment)

-- We can use the stateful computation pattern we taught in the class to implement our
-- interpreters. Here is the implementation for Plus case.
evaluate (Plus a b) =
  evaluate a `andThen` \va ->
  evaluate b `andThen` \vb ->
  pureValue (binLift (+) va vb)

-- Please finish the implementation for the Minus case and the constant literal case. 
evaluate (Minus a b) =
    evaluate a `andThen` \va ->
    evaluate b `andThen` \vb ->
    pureValue (binLift (-) va vb)

evaluate (Lit v) = pureValue (Just v)
-- For variable refrence, we will first use "get" primitive to fetch the environment.
-- And then we will call the locate function to find the corresponding value.
-- Please finish the implementation.
evaluate (Var v) = 
    get `andThen` \env ->
    pureValue (locate env v)
-- For assignment, we will recursively evaluate the sub-expression and then call
-- the bindVar function to update the environment with put. Note that you may need
-- to handle the case specially when the sub-expression triggers error and returns
-- Nothing.
-- Please finish the implementation.
evaluate (Assign v a) = 
    evaluate a `andThen` \value -> 
    (handleAssignment v value)
  
-- We will implement the evaluation of a list of statements with a subroutine.
evaluate (BlockExprs stmts) = evaluateStmt stmts
-- To implement loop, we will evaluate the expression once, check the return value
-- and see whether it is zero. If it is not, then we will recursively evaluate the loop
-- expresison again.
--
-- Please finish the implementation
evaluate (Loop (BlockExprs [])) = pureValue (Just 0)
evaluate (Loop (BlockExprs stmts)) = 
    (evaluate (BlockExprs stmts)) `andThen` \value ->
    (\s -> (if (isResultZero value)
            then (pureValue value s)
            else (evaluate (Loop (BlockExprs stmts)) s)))

-- Please also finish the implementation of the evaluateStmt function. Remember that
-- it should return the value of the last expression in the list as its own value.
evaluateStmt [] = pureValue (Just 0)
evaluateStmt (x:[]) = evaluate x `andThen` \value -> pureValue value
evaluateStmt (x:xs) = evaluateStmt [x] `andThen` \value -> evaluateStmt xs

-- If you finished the above implementation correctly, you will be able to compile the
-- program with ghc. Running this program will generate the following output.
--
-- Just 20
-- Nothing
-- Just 500500
--
-- Please test your program more before submission. Just remember that when you submit your
-- program, make sure put your own testcases after offcial ones, i.e., make sure that the
-- first three lines of your program matching the expected output.

prog1 = BlockExprs [Assign "z" (Lit 10), Plus (Assign "x" (Var "z")) (Minus (Lit 20) (Var "x"))]
prog2 = BlockExprs [Assign "z" (Lit 0), Assign "x" (Plus (Lit 10) (Var "x"))]
prog3 = BlockExprs [Assign "x" (Lit 1000), Assign "s" (Lit 0), Loop (BlockExprs [Assign "s" (Plus (Var "s") (Var "x")), Assign "x" (Minus (Var "x") (Lit 1))]), Var "s"]

main = do
  print (fst (evaluate prog1 []))
  print (fst (evaluate prog2 []))
  print (fst (evaluate prog3 []))




