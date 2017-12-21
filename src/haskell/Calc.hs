module Main where
import Grammar
import Tokens

"""
An interesting idea for embedding execution logic would be to translate 
 expressions into a push-down automota bytecode. That way we don't have to
 traverse complex datastructures to do computation logic. 

Definitions

    Opcodes: An instruction that is byte encodable. They have either explicit
        constants embedded into the opcodes, otherwise all of the arguments are
        implicitly popped from the stack.
    
    Stack: A stack of values. Whether the stack is only as bytes, or represented
        as individual typed values is to be determined.

What this might look like could be:

Expression:
    WHEN round_down((5 + \"price\") / 100)
        CASE 1 THEN 5
        CASE 2 THEN 6
    END

Expression opcodes:
    LOAD items.price // tblID.colID
    PLUS 5 // Implicitly operate on the last item, top of the stacks
    DIV 100
    FUNC round_down // funcID
    CMP 1 +3
    CMP 2 +3
    RET NULL // Alias (or alternatively) for PUSH NULL
    RET 5
    RET 6

Given this conversion, it's also fairly easy to create complex sub-expressions
and combine them with a stack.
    <Complex sub-expression one>
    <Complex sub-expression two>
    ADD // Use top 2 elements

An observation of performance properties:
    We know ahead of time the size of the maximum stack required by the amount
    of 'active' PUSHs (unpopped pushes). This is not very well observable due to
    opcodes having implicit stack pops.

        - This warrants explicit stack pops in annotations, e.g.
            `ADD 1` could instead look like `ADD 1, $0`

        - An even stronger guarantee would be to annotate types, as the stack
          operations vary by the types (string vs. int vs. float).

        - A problem with this would be how the order of arguments is specified.
          I'm unsure if the construction of the bytecode suffices here.

    Another interesting property may be the ability ot batch computation.
    Instead of running through the same bytecode over and over, we can possibly
    maintain multiple stacks simultaneously for computing the results of the
    same expression over multiple values. Adversely, we may see performance
    problems here. Unless we can translate the expression code into highly
    cachable code (very tight ASM loop?)

    A potentially harmful problem is the conditional retrieval of a 
    column value. For example, WHEN x CASE 1 \"v1\" CASE 2 \"v2\" END.
    We could more optimmally determine when to pull the column values,
    based on pulling the vallues in a second batch and possibly even
    preventing computation until then.

Use case, or problematic area:
    We can pre-compute the expression bytecode before sending work off to other
    nodes, but we will need to run these in potentially very tight loops for
    complex FILTER expressions. 

"""

-- Types
-- (simple types)	represents the type of that name
-- TArray	array type
-- TTuple	tuple type
-- TTable	table type ("setof")

-- Expression nodes
-- NumVal	exact numeric literal, evaporates after type checking
-- StrVal	exact string literal, evaporates after type checking
-- (other leaf datum nodes)	SQL values
-- Default	special value DEFAULT used in INSERT
-- Placeholder	for $1 etc in prepared statements
-- ColName	named reference to a column from the current source, evaporates into IndexedVar after name resolution
-- IndexedVar	for @1 etc - ordinal column references

-- Subquery expr
-- Subquery	a table node used as expression

-- Composite constructorsTuple	tuple literal constructor
-- Array	array literal constructor
-- SingleRow	node wrapped around a subquery to extract its only row of results or a NULL tuple if there were no results

-- Type assertions with optional run-time conversion
-- AnnotateType	type enforcement during type inference
-- Cast	type conversion at eval time
-- Collate	string collation

-- Conditional expressions:
-- Case	case when else (also used for IF)
-- IsOf	run-time type test
-- NullIf	NULLIF(...)
-- Coalesce	first non-NULL value

type Env = String -> Exp
emptyEnv = error "Not found"
envLookup s env = env s
envBind s v env = (\s' -> if s == s' then v else env s)

eval :: Exp -> Env -> Int
eval (Int v) _         = v
eval (Plus e1 e2) env  = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env   = (eval e1 env) `div` (eval e2 env)
eval (Negate e) env    = -(eval e env)
eval (Var s) env       = eval (envLookup s env) env
eval (Let s e1 e2) env = eval e2 env'
    where env' = envBind s e1 env
    
run :: Exp -> Int
run e = eval e emptyEnv

type ContextAccum = (Exp -> Env) -> Env
type ValueAccum = (Exp -> Env -> Any<T>) -> Any<T>
type WalkTransform = (Exp -> Env -> Any<T>) -> Exp

type WalkFuncs = (ContextAccum, ValueAccum, WalkTransform)

-- Walk function
-- Walk along the AST IR allowing a different modes of operation
--  - Accumulate a result
--  - Transform the IR (replace nodes, embed additional data)
-- The walk function will always accumulate a context. This embeds data such as
--  available variables, table scopes, etc.
-- Essentially Env is the accumulation of data down the tree, and Any<T> is the
--  accumulation of data up the tree to the root, as an accumulated value.
-- NOTE: there may be an obvious win for optimization by caching the context
--  across walks. There may be creative ways to use one store, otherwise 
--  niavely you will store a large set of contexts.
-- NOTE: Any<T> must be concatable / combinable.
walk :: WalkFuncs -> Exp -> Env -> (Exp, Any<T>)
-- TODO: this is our new result funtion. Multiple expressions just need to 
--   concat the results of walking on all children
walk (cAccum, vAccum, wTrans) (Int v) env       = (tExp, val')
    where exp = (Int v)
          env' = cAccum exp env
          -- FIXME: this only happens when walking multiple children
          --   (exp', val) = walk (cAccum, vAccum, wTrans) exp env'
          tExp = wTrans exp env' val
          val' = vAccum tExp env' val
walk (cAccum, vAccum, wTrans) (Negate v) env    = (f (Negate v) empty env)
walk (cAccum, vAccum, wTrans) (Var v) env       = (f (Var v) empty env)
walk (cAccum, vAccum, wTrans) (Plus e1 e2) env  = (f e1 empty env) ++ (f e2 empty env)
walk (cAccum, vAccum, wTrans) (Minus e1 e2) env = (f e1 empty env) ++ (f e2 empty env)
walk (cAccum, vAccum, wTrans) (Times e1 e2) env = (f e1 empty env) ++ (f e2 empty env)
walk (cAccum, vAccum, wTrans) (Div e1 e2) env   = (f e1 empty env) ++ (f e2 empty env)
walk (cAccum, vAccum, wTrans) (Let s e1 e2) env = eval e2 env'
    where env' = envBind s e1 env

"""
Idea behind walk:
A context accumulator and a value accumulator. These can be chained
when done separately
"""

-- GatherColumns collects all referenced column IDs
GatherColumns :: ContextAccum
-- If colum reference node
ReturnColumns (Column colID) env val = envBind "columns" $ columns ++ [colID]
    where columns = envLookup "columns" env 
-- If non-column reference node
ReturnColumns _ env = env

-- ReturnColumns pulls the result of GatherColumns in the leafnode
-- TODO: default value for input?
ReturnColumns :: ValueAccum
-- If leaf node
ReturnColumns (Leaf _ ...) env val = valBind "columns" $ envLookup "columns" env
-- If non-leaf node
ReturnColumns _ _ val = val

walkIdentiy :: (Exp -> Any<T> -> Env) -> (Exp, Any<T>)
walkIdentiy (Int v) _ _ = ((Int v) empty)
walkIdentiy (Negate v) _ _ = ((Negate v) empty)
walkIdentiy (Var v) _ _ = ((Var v) empty)
walkIdentiy (Plus e1 e2) _ _ = ((Plus e1 e2) empty)
walkIdentiy (Minus e1 e2) _ _ = ((Minus e1 e2) empty)
walkIdentiy (Times e1 e2) _ _ = ((Times e1 e2) empty)
walkIdentiy (Let s e1 e2) _ _ = ((Let s e1 e2) empty)

dereference :: Exp -> Env -> Exp
eval (Int v) _         = v
eval (Plus e1 e2) env  = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Times e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Div e1 e2) env   = (eval e1 env) `div` (eval e2 env)
eval (Negate e) env    = -(eval e env)
eval (Var s) env       = eval (envLookup s env) env
eval (Let s e1 e2) env = eval e2 env'
    where env' = envBind s e1 env

main :: IO ()
main = do
    s <- getContents
    let ast = parseCalc (scanTokens s)
    print ast
    print (run ast)
    