module Interpreter where

import AbstractSyntax

-- Takes a program represented as an abstract syntax tree and executes it line by line

{- Storage -}

--Store variables as a function from identifier to value
type Storage = Identifier -> Integer

emptyStorage :: Storage
emptyStorage i = error ("Uninitialized variable " ++ i)

-- Updating storage
-- Create a new storage where the identified variable has new value val
update :: Identifier -> Integer -> Storage -> Storage
update i x st = st'
    where
        st' :: Storage -- new storage
        st' i' | i == i'   = x     -- If the identifier matches ident reutrn the new value
               | otherwise = st i' -- otherwise return the origonal value


-- Booleans

number :: Bool -> Integer
number False = 0
number True  = 1

boolean :: Integer -> Bool
boolean 0 = False
boolean _ = True

{- Evaluating Expressions -}

{- Expressions must be evaluated relative to the storage
 - An Expression is either
 - 	a constant
 - 	a variable
 - 	or an operator with a list of sub expressions
 -
 - When evaluating for st :: Storage
 - 	A Constant means extract the integer value and store it
 - 	A Variable `x` means look up the value `m "x"`
 - 	An Operator means apply Haskell operations associated with that operator
 - 	to the evaluated sub-expressions.
 -}

eval :: Storage -> Expr -> Integer
eval st (Constant x) = x                              -- An expression can be a constant
eval st (Var i)      = st i                           -- An expression can be a stored variable
eval st (Op o es)    = opEval o [eval st e | e <- es] -- An expression can be an operation on sub expressions which needs evaluating

--Evaluating operations using the OpName defined in AbstractSyntax
opEval :: OpName -> [Integer] -> Integer
opEval Add     [x, y] = x + y
opEval Sub     [x, y] = x - y
opEval Mul     [x, y] = x * y
opEval Div     [x, y] = x `div` y
opEval Mod     [x, y] = x `mod` y
opEval Eq      [x, y] = number(x == y)
opEval Leq     [x, y] = number(x <= y)
opEval Less    [x, y] = number(x <  y)
opEval Geq     [x, y] = number(x >= y)
opEval Greater [x, y] = number(x >  y)
opEval And     [x, y] = number(boolean x && boolean y)
opEval Or      [x, y] = number(boolean x || boolean y)
opEval Not     [x]    = number(not(boolean x))
opEval op      xs     = error ("Interpreter bug. "
                            ++ "Please contact the software maintainer. "
                            ++ "Tried to apply " ++ show op
                            ++ " to " ++ show xs)

{- Executing a program -}

-- A Program maps some input storage to some output storage.
-- So run must take a program and an inital storage
-- and produce a final storage if it is sucessfull

run :: Program -> Storage -> Storage

--  Evaluate expression from storage and update storage with the new value
run (i := e) st = update i (eval st e) st

run (IfElse condition p1 p2) st
    | boolean (eval st condition) = run p1 st -- If Condition then run p1 on storage
    | otherwise                   = run p2 st  -- Else run p2 on storage

run (If condition p) st
    | boolean (eval st condition) = run p st -- Condition met run program on storage
    | otherwise = st -- If condition not met do nothing to storage

run (While condition p) st
    | boolean (eval st condition) = st'' -- Execute the program on the storage untill condition not met
    | otherwise                   = st   -- Condition not met do nothing to storage
    where
        st'  = run p st
        st'' = run (While condition p) st' -- Recurse untill condition not met

run (Block []) m = m -- Empty block do nothing to storage

-- 
run (Block (p:ps)) st = st''
    where
        st'  = run p st           -- Run the first program in the block 
        st'' = run (Block ps) st' -- Run the remaining programs in the block


{- ALTERNATIVE Executing programs using Storage Transformers -}

{-
 - We can view the above approach as a function which takes a program and provides a "storage transformation"
 - This is a function of type Storage -> Storage which alters the stored values depending on the program
 - We can generalize this approach and gain a more consise definition of run
 -}

-- Assign transforms a store by updating a value
assign :: Identifier -> Expr -> Storage -> Storage
assign i e = \st -> update i (eval st e) st

{- An If Else requires
- 	A predicate, p,  on a storage
- 	A sucessfull transformation, f, to apply
- 	An alternative transformation, g, to apply
-
- It will then return the sucess or alternate transformation
- iAlthough s is nominally Storage, we can use the generalision s in this case as this can actually be applied to any type
-}

ifElse :: (s -> Bool) -> (s -> s) -> (s -> s) -> (s -> s)
ifElse p f g = tf -- Return transformation
    where
        tf st = if p st then f st else g st

-- If is simply an if else where the alternative transformation if id : a->a which leaves an input unchanged

-- While once again takes a predicate on s, it then takes a transformation to apply
-- untill p is no longer satisfied
while :: (s->Bool) -> (s->s) -> (s->s)
while p f = g
    where
        g st = if p st then g (f st) else st


-- We can now construct blocks simply as a series of transformations to apply
block :: [s -> s] -> s -> s
block []     st = st
block (f:fs) st = block fs (f st) -- Apply f and recurse onto fs

-- Boolean expressions are predicates on the store
booleanValue :: Expr -> (Storage -> Bool)
booleanValue e = \st -> boolean (eval st e)

-- Finally we can define a permutation of our run function using storage transformations
run' :: Program -> Storage -> Storage
run' (i := e)       = assign i e
run' (IfElse e p q) = ifElse (booleanValue e) (run' p) (run' q)
run' (If e p)       = ifElse (booleanValue e) (run' p) id
run' (While e p)    = while  (booleanValue e) (run' p)
run' (Block ps)     = block  (map run' ps)















