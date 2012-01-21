
module Backend where

import Katalog (Fact, Rule, Atom, Term, Datalog, Subst, atomName, atomId, unifyAtom)

import Control.Monad
import Data.Maybe
import Data.List

type Name = String

-- f could be State Datalog
class Monad f => Backend f where
  
  -- the list of all facts, including derived rules 
  facts :: f [Fact]
  
  -- the list of all facts for the given name 
  factsFor :: Name -> f [Fact]
  factsFor n = liftM (filter (\x -> atomName x == n)) facts 

  factsForId :: Int -> f [Fact]
  factsFor n = liftM (filter (\x -> atomId x == n)) facts 

  -- the list of all rules
  rules :: f [Rule]

  -- the list of all rules for a given table 
  rulesFor :: Name -> f [Rule]

  -- only memoize facts for the given table
  -- memo :: Name -> f ()

  -- hint to the backend that it should memoize derived facts for the given Name 
  memoAll :: f () 

  -- returns the first set of variable bindings for which the assertion holds, 
  -- or Nothing if no facts unify with the given query
  query :: Atom Term -> f (Maybe Subst)
  query q = do 
    fs <- facts
    return $ join (find isJust (map (\f -> unifyAtom [] q f) fs))

  -- add new facts and rules to knowledge base
  declare :: Datalog -> f () 


