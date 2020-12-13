module AbstractSyntax where

{- Define the syntax of the language in Haskell -}

-- Define Identifier as a type synonym for a string
type Identifier = String

-- Define the operations of the language
data OpName = Or                                 --  ||
            | And                                --  &&
            | Eq                                 --  ==
            | Leq | Less | Geq | Greater         --  <=  <  >=  >
            | Add | Sub                          --  +  -
            | Mul | Div | Mod                    --  *  /  %
            | Not                                --  !
            deriving (Show)

-- define Expressions that can be evaluated by the language
data Expr = Constant Integer
          | Var Identifier
          | Op OpName [Expr]
          deriving (Show)

-- Finally define all the combinations of blocks statements and expressions which can be a valid program
data Program = Identifier := Expr -- `:=` is a data constructor used to construct an Expression following our identifier 
             | Block [Program]
             | While Expr Program
             | If Expr Program
             | IfElse Expr Program Program
             deriving (Show)

