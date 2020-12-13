# A Small Imperative Language called XY
This is an example from the University Of Birmingham School of Computer Science's Functional Programming Module.

This program is an intepreter written in Haskell using Parser Combinators to translate and execute instructions in the language. The language has no form of IO commands and so accepts an integer input value x and a output variable y hence it is the XY Interpeter.

**Executing a program**
> runhaskell Runxy <program>.xy <Integer x>
y

## Structure

Given a `String` containing a wellformed XY program in the given *concrete syntax*
- The program is parsed to produce a program tree in abstract syntax
- This will be used with an initial storage , an assignment of values to program variables, the program will be ran to produce a new storage

                                                                 | initial
                                                                 | storage
                                                                 v           final
   source code        +-----------+   program tree       +-------+--------+  storage
--------------------->| Parser.hs | -------------------->| Interpreter.hs | -------->
  (concrete syntax)   +-----------+  (AbstractSyntax.hs) +----------------+
  (a string)


## Concrete Syntax

The following is the grammar of the language produced using BNF. A String well formed in this gramamr will be parsed using monadic parsing to produce our syntax tree.

```
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
```

Note the grammar is ambiguous to else, which is resolved by associating else ot the closest `if` on it's left
