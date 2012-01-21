{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Katalog where
-- a minimalist datalog for use by kata
-- stolen from Ed Kmett
import Backend

import qualified Data.List as L

import Control.Arrow ((&&&),(***))
import Control.Applicative ((<$>))
import Control.Monad.State

import Data.Monoid
import Data.Graph

combine :: Datalog -> Datalog -> Datalog
combine a b = g (mappend a b) where
  g (x, y) = (L.nub x, L.nub y)


subst :: (Functor m, Monad m) => Atom Term -> Subst -> m (Atom Con)
subst (Atom pred args) vars = Atom pred <$> sequence args' where
    args' = eitherTerm (maybe (fail "bad subst") return . flip lookup vars) return <$> args
    
-- obtain new facts, note, that all lesser stratified rules must be evaluated by now or NOT'd patterns will oscillate
evalRule :: [Fact] -> Rule -> [Fact]
evalRule facts (Rule h pats) = do
    s' <- matches pats []
    guard $ null $ unwanted s' -- check negations
    f <- subst h s'
    guard $ notElem f facts
    return f
  where
    matches :: [Pat] -> Subst -> [Subst]
    matches [] s = return s
    matches (Not _:ps) s = matches ps s
    matches (Pat pat:ps) s = do
        x <- facts
        y <- unifyAtom s pat x
        matches ps y

    nots :: [Atom Term] 
    nots = patAtom <$> filter isNot pats

    unwanted :: Subst -> [Subst]
    unwanted sub = do
        n <- nots
        f <- facts
        unifyAtom sub n f

addFact :: Fact -> [Fact] -> [Fact]
addFact f fs | f `elem` fs = fs
             | otherwise = f:fs

-- NOT VALID IN THE PRESENCE OF NOT!
naive :: [Fact] -> [Rule] -> [Fact]
naive edb rules 
    | null delta = edb
    | otherwise = naive (foldr addFact edb delta) rules
    where 
        delta :: [Fact]
        delta = rules >>= evalRule edb
    

-- NOT VALID IN THE PRESENCE OF NOT!
seminaive :: [Fact] -> [Rule] -> [Fact]
seminaive f r = seminaive' f r f

seminaive' :: [Fact] -> [Rule] -> [Fact] -> [Fact]
seminaive' edb rules hot 
    | null delta = edb
    | otherwise = seminaive' (foldr addFact edb delta) rules delta
    where 
        affects :: Fact -> Rule -> Bool
        affects (Atom (C c _) _) (Rule _ body) = any ((c==) . conId . atomPred . patAtom) body where

        relevant :: Rule -> Bool
        relevant rule = any (flip affects rule) hot

        delta :: [Fact]
        delta = filter relevant rules >>= evalRule edb
    
-- Stratified Datalog
newtype Min a = Min { runMin :: a } deriving (Show,Read,Ord,Eq,Bounded)
instance (Bounded a, Ord a) => Monoid (Min a) where
    mempty = maxBound
    mappend = min

newtype Max a = Max { runMax :: a } deriving (Show,Read,Ord,Eq,Bounded)
instance (Bounded a, Ord a) => Monoid (Max a) where
    mempty = minBound
    mappend = max

ruleBounds :: Rule -> (Min Vertex,Max Vertex)
ruleBounds (Rule head body) = atomBounds head `mappend` mconcat (patBounds <$> body) where
    atomBounds = (Min &&& Max) . conId . atomPred
    patBounds = atomBounds . patAtom

ruleEdges :: Rule -> [Edge]
ruleEdges (Rule head body) = (,) h <$> bodyEdges <$> body where
    h = conId $ atomPred head
    bodyEdges = conId . atomPred . patAtom

ruleGraph :: [Rule] -> Graph
ruleGraph rules = buildG bds edges where
    bds = (runMin *** runMax) $ mconcat (ruleBounds <$> rules)
    edges = rules >>= ruleEdges

-- TODO: 
-- check sccs, foreach scc, check to see if there is any negated edge within the scc, if so, the idb isn't stratified
-- we can go a long way with the idea of active rules, and adding more active rules in topological order
--
-- TODO:
-- quickcheck properties, arbitrary instances
-- runP <parser> = PP.render doc x
--

data DB = DB { fullyExtended :: Bool, db :: Datalog }   

instance Monoid DB where
  mempty = DB True mempty
  mappend a b = DB False (db a `mappend` db b)

-- return all derived facts, but don't commit them
derive :: State DB DB
derive = do 
  DB b db <- get
  return $ if b then DB b db else DB True (combine db ((uncurry seminaive db, [])))

instance Backend (State DB) where
   facts = liftM (fst . db) derive 
   rules = liftM (snd . db) derive 
   memoAll = derive >>= put
   declare db = modify (\(DB _ db0) -> DB False (combine db0 db)) 
