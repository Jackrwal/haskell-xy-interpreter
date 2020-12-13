module Parser where

import Data.Char
import Control.Monad

import AbstractSyntax
import Parsing

{- The following is the grammar which is defined in AbstractSyntax described in BNF
Program ::= Identifier := Expr;
          | { [Program] }
          | while (Expr) Program
          | if (Expr) Program
          | if (Expr) Program else Program

Expr  ::= Expr1 | Expr1 OrOp   Expr
Expr1 ::= Expr2 | Expr2 AndOp  Expr1
Expr2 ::= Expr3 | Expr3 EqOp   Expr2
Expr3 ::= Expr4 | Expr4 CompOp Expr3
Expr4 ::= Expr5 | Expr5 AddOp  Expr4
Expr5 ::= Expr6 | Expr6 MulOp  Expr5
Expr6 ::= Expr7 | NotOp Expr6
Expr7 ::= Constant | Identifier | (Expr)

OrOp   ::=  ||
AndOp  ::=  &&
EqOp   ::=  ==
CompOp ::=  <=  |  <  |  >=  |  >
AddOp  ::=  +   |  -
MulOp  ::=  *   |  /  |  %
NotOp  ::=  !
-}

{- Define Parsers we need -}

-- Parses a program by the BNF definition above. produceing a Program tree in a parser
program :: Parser Program

-- Parse the accepted expressions in our grammar
expr, expr1, expr2, expr3, expr4, expr5, expr6, expr7 :: Parser Expr

-- Parse operations between expressions
orOp, andOp, eqOp, compOp, addOp, mulOp, notOp :: Parser ([Expr] -> Expr)


{- Implementing Parsers -}

{- Things To Remember About Parsers. 
 -  Parser Returns a function from a string. so must be applied to a string (using Parse which extracts this function from the type)
 -  The sequencing of Parses means the remainder of the previous Parser is automatically fed into the next
 -  If any parser fails at any point the error is automatically propogated
 -}

{- Parser -}

-- Use program parser to parse the string
parseProgram :: String -> Program
parseProgram xs = case parse program xs of
                   [(p, [])] -> p
                   [(_, s)]  -> error ("Syntax: unpasred string" ++ s)
                   _         -> error "Syntax: Failed to parse program"
                   

{- Program Parsers -}

-- Define the Alternatives our program can contain. Acording to the BNF a program can be;
program =
      assignment       -- An Assignment,
  <|> block            -- A Block containing programs,
  <|> whileStatement   -- A White Statement,
  <|> ifStatement      -- Or and If statement

-- Defining an Identifier

-- Defining Expression Parsers

-- Defining a parser for assignment
-- The productiom rule defines assignment as an identifier constructed with an expression (Identif := Expr)
assignment = 
  do
    i <- identif    -- Extract the identifier
    symbol ":="     -- Consume the assignment operator symol
    e <- expr       -- Parse the expression
    symbol ";"      -- Consume a trailing ';' symbol
    return (i := e) -- Construct an assignment

keywords = ["if", "else", "while"]

identif :: Parser String
identif =
  do
    cs <- token identifier        -- Extract the identifier 
    guard (not (elem cs keywords)) -- Identifier cannot be one of our keywords
    return cs

-- Defining a parser for Block
-- A Block's production rule is a list of programs enclosed in curly brackets `{ [Program] }
block = 
  do
    symbol "{"
    ps <- many program -- extract Zero or more matches to program
    symbol "}"
    return (Block ps)  -- Construct the list of programs as a Block

-- Defining a parser for a whileStatement
whileStatement = 
  do
    symbol "while"
    symbol "("
    e <- expr    -- Parse and extract the condition
    symbol ")"
    p <- program -- Parse and extract the program to loop
    return (While e p) -- Construct a White type with the condition and program

-- Defining a parser for an ifStatement
ifStatement = 
  do
    symbol "if"
    symbol "("
    e <- expr
    symbol ")"
    p1 <- program
    ((do -- Atempt to Parse an else statement
        symbol "else"
        p2 <- program
        return (IfElse e p1 p2)) -- If sucessfull construct an If Else
      <|>
        (return (If e p1)))      -- if fails to construct else construct an If


{- Expression Parsers -}

-- The BNF defines an expression as either an expression or an operation on two expressions
-- expr := expr' | expr' op expr
-- Where it then excplicitly numbers and defines the types of expression

-- Takes two expression parsers and an operation parser to attempt to pare the expression as an operation
binExpr :: Parser e -> Parser ([e] -> e) -> Parser e -> Parser e
binExpr expr' op expr = 
  do
    e' <- expr' -- Parse the first expression

    ((do -- Attempt to parse an expression `e op e`
        o <- op   -- Parse the operator
        e <- expr -- Parse the second expression
        return (o [e',e])) -- If sucessfull construct an operation and reutrn it
                          -- e can be of either expr' or expr' op expr so type e can be an operation
      <|>
        return e') -- If the expression is just of the form expr' return it as an expression 


-- We can now use this to define all of our binary operations on epxressions
expr  = binExpr expr1 orOp   expr
expr1 = binExpr expr2 andOp  expr1
expr2 = binExpr expr3 eqOp   expr2
expr3 = binExpr expr4 compOp expr3
expr4 = binExpr expr5 addOp  expr4
expr5 = binExpr expr6 mulOp  expr5

-- expr6 is either an expr7 or a unary NOT operation
expr6 = expr7 <|>
        do
          op <- notOp
          e  <- expr6
          return (op [e]) -- Construct and return unary operation

-- expr7 is a constant, a variable identifier, or an expression in perenthesies 
expr7 = constant <|>
        do
          i <- identif
          return (Var i) -- Construct a variable with the identifier and return
        <|>
        do 
          symbol "("
          e <- expr      -- parse the expression
          symbol ")"
          return e       -- return the expression


-- Parse numerical constants
constant :: Parser Expr
constant = 
  do
    n <- integer -- Parse an integer using Parsing.hs
    return (Constant(toInteger n))         


-- We must also implement the operations which our expression parasers use

-- Parse an operator generically
parseOp :: String -> OpName -> Parser ([Expr] -> Expr)
parseOp s op =
  do
    symbol s       -- Extract the operator
    return (Op op) -- Construct as an operator

-- Parsse specific operations
orOp  = parseOp "||" Or
andOp = parseOp "&&" And
eqOp  = parseOp "==" Eq

compOp = parseOp "<=" Leq
     <|> parseOp "<"  Less
     <|> parseOp ">=" Geq
     <|> parseOp ">"  Greater

addOp = parseOp "+" Add
    <|> parseOp "-" Sub

mulOp = parseOp "*" Mul
    <|> parseOp "/" Div
    <|> parseOp "%" Mod

notOp = parseOp "!" Not










