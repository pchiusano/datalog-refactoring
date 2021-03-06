module Backend where

import Control.Monad
import Data.Maybe
import Data.List

import qualified Data.List as L

import qualified Data.Map as M
import Data.Map (Map)
import Data.List

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on (*) f x y = f x * f y

type Name = String

type Id = Int

newtype Var = V String deriving (Show,Eq,Ord)  -- x
varName (V s) = s

data Con = C Id String deriving (Show) -- Foo
conName (C _ s) = s
conId (C x _) = x

instance Eq Con where
    (==) = (==) `on` conId 
    (/=) = (/=) `on` conId

instance Ord Con where
    compare = compare `on` conId

data Term = Var Var | Con Con deriving (Show,Eq,Ord)

data Atom t = Atom Con [t] deriving (Show,Eq,Ord) --{ atomPred :: Con, atomTerms :: [t] } deriving (Show,Eq)

type Fact = Atom Con
type Datalog = ([Fact], [Rule])
type Subst = [(Var,Con)]

atomPred :: Atom t -> Con
atomPred (Atom x _) = x
atomArgs :: Atom t -> [t]
atomArgs (Atom _ t) = t
atomName :: Atom t -> String
atomName (Atom (C _ s) _) = s
atomId :: Atom t -> Int
atomId (Atom (C id _) _) = id

data Pat = Not (Atom Term) | Pat (Atom Term) deriving (Show,Eq,Ord)
patAtom :: Pat -> Atom Term
patAtom (Pat a) = a 
patAtom (Not a) = a

isNot :: Pat -> Bool
isNot (Not _) = True
isNot _ = False

data Rule = Rule (Atom Term) [Pat] deriving (Show,Eq,Ord)
ruleHead :: Rule -> Atom Term
ruleHead (Rule x _) = x

ruleBody :: Rule -> [Pat] 
ruleBody (Rule _ x) = x

eitherTerm :: (Var -> a) -> (Con -> a) -> Term -> a
eitherTerm f _ (Var x) = f x
eitherTerm _ g (Con y) = g y


unifyAtom :: (Functor m, Monad m) => Subst -> Atom Term -> Fact -> m Subst
unifyAtom s (Atom b ps) (Atom h cs) 
    | h == b    = unifyList s ps cs
    | otherwise = fail "predicate mismatch"

unifyCon :: (Functor m, Monad m) => Subst -> Con -> Con -> m Subst
unifyCon s c c' | c == c' = return s
                | otherwise = fail "constructor mismatch"

unifyList :: (Functor m, Monad m) => Subst -> [Term] -> [Con] -> m Subst
unifyList s (Con p:ps) (c:cs) = unifyCon s p c
unifyList s (Var v:ps) (c:cs) = case L.lookup v s of
    Just c' -> unifyCon s c c'
    Nothing -> return $ (v,c):s
unifyList s [] [] = return s
unifyList s _ _ = fail "arity mismatch"

-- f could be State Datalog
class Monad f => Backend f where
  
  -- the list of all facts, including derived rules 
  facts :: f [Fact]
  
  -- the list of all facts for the given name 
  factsFor :: Name -> f [Fact]
  factsFor n = liftM (filter (\x -> atomName x == n)) facts 

  factsForId :: Int -> f [Fact]
  factsForId n = liftM (filter (\x -> atomId x == n)) facts 

  -- the list of all rules
  rules :: f [Rule]

  -- the list of all rules for a given table 
  rulesFor :: Name -> f [Rule]
  rulesFor n = liftM (filter (\r -> atomName (ruleHead r) == n)) rules

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

