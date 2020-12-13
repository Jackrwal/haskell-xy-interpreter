module Main where

import System.Environment

import AbstractSyntax
import Parser
import Interpreter


-- CLI to execute xy language files with the extention .xy
-- which should contain a well formated string in the language's grammar

-- Initialize the storage with the CLI argument x
initialStorage :: Integer -> Storage
initialStorage x = update "x" x emptyStorage 

-- Runs an xy program using the xyIntepreter.
runxy :: Program -> Integer -> Integer
runxy p x = st' "y"
    where
        st  = initialStorage x
        st' = run p st

main :: IO ()
main = 
  do
    args <- getArgs            -- Get CLI Args
    if length args == 2 then   -- check right number of args
      do
        concreteProgram <- readFile $ args !! 0            -- Read the program file
        let abstractProgram = parseProgram concreteProgram -- Parse the program for an abstract syntax tree
        let x = read $ args !! 1                           -- get the x argument
        let y = runxy abstractProgram x                    -- execute the program with the interpreter
        putStrLn (show y)                                  -- output Y
    else
      putStrLn "Usage: Runxy <filename> <Int>"
