{- 
 - plan
 -   * implement/steal datalog interpreter, accessible from Haskell somehow
 -     * should have some AST for Datalog, a function from this AST to some underlying repr
 -     * do something similar to what we do for backend - a typeclass for the AST interpreter
 -     * expose a nice REPL in Haskell for a datalog database 
 -   * write a toy language, toy parser
 -   * write round trip for this language
 -     * code to convert language AST to datalog AST
 -     * code to convert from datalog AST to language AST
 -   * write datalog query for transforming the AST  
 -   * goal - two phase REPL - can add bindings to the programming environment
 -
 -
 -   $ f x y = x + y
 -   $ :datalog
 -   $ ap(Id, X) :- names(
 -
 -}

module Main where

import REPL
import Data.Monoid
import Katalog

main :: IO ()
main = do 
  io <- stateLowerIO (DB True mempty)
  repl io 



