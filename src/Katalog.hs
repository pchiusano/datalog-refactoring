{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Katalog where
-- a minimalist datalog for use by kata

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (($$),(<>),(<+>))

import qualified Data.Text as T
import Data.Text (Text)

import qualified Data.List as L

import qualified Data.Map as M
import Data.Map (Map)

import Control.Arrow ((&&&),(***))
import Control.Applicative ((<$>))
import Control.Monad.State

import Data.Monoid
import Data.Graph
import Data.Either (partitionEithers)

import Text.Parsec.Combinator
import Text.Parsec.Prim hiding (State)
import Text.Parsec.Error
import Text.Parsec.Char

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on (*) f x y = f x * f y

instance (Monad m) => Stream Text m Char where
    uncons = return . T.uncons

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
atomPred :: Atom t -> Con
atomPred (Atom x _) = x
atomArgs :: Atom t -> [t]
atomArgs (Atom _ t) = t

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

type Fact = Atom Con
type Datalog = ([Fact], [Rule])

data Env = Env 
    { envConMap :: Map String Con
    , envNextFree :: !Id
    } deriving (Show)

type P = Parsec Text Env

fresh :: P Int
fresh = do
    env <- getState
    let free = envNextFree env
    putState $ env { envNextFree = free + 1 }
    return free

mkCon :: String -> P Con
mkCon k = do
    m <- envConMap <$> getState
    case M.lookup k m of
        Just c -> return c
        Nothing -> do
            i <- fresh
            let result = C i k
            modifyState $ \env -> env { envConMap = M.insert k result m } 
            return result

-- parser

word :: P String
word = many1 letter

var :: P Var
var = do
    l <- upper <?> "variable"
    ls <- many letter
    return $ V (l:ls)

con :: P Con
con = do
    u <- lower <?> "constructor"
    us <- many letter
    mkCon (u:us)

neg :: P ()
neg = (string "\\+" <|> string "~") >> return ()

term :: P Term
term =  Var <$> var 
    <|> Con <$> con

eitherTerm :: (Var -> a) -> (Con -> a) -> Term -> a
eitherTerm f _ (Var x) = f x
eitherTerm _ g (Con y) = g y

turnstile :: P ()
turnstile = string ":-" >> return ()

period :: P ()
period = char '.' >> return ()

comma :: P ()
comma = char ',' >> return ()

open :: P ()
open = (char '(' >> spaces >> return ()) <?> "("

close :: P ()
close = (spaces >> char ')' >> return ()) <?> ")"

betweenParens :: P a -> P a
betweenParens = between open close 

spaced :: P a -> P a
spaced = between spaces spaces

atom :: P a -> P (Atom a)
atom t = do
    pred <- con
    Atom pred <$> (betweenParens (t `sepBy` spaced comma) <|> return [])
  <?> "atom"

pat :: P Pat
pat = do { neg; spaces; Not <$> atom term } <|> Pat <$> atom term
          
fact :: P (Atom Con)
fact = atom con
  <?> "fact"

rule :: P Rule
rule = do
    head <- atom term
    spaced turnstile <?> ":-"
    body <- pat `sepBy` many1 space
    safe $ Rule head body
  <?> "rule"

safe :: Rule -> P Rule
safe rule@(Rule head body) = do
        forM_ headVars $ \v -> 
            when (v `notElem` bodyVars) $ do
                unexpected $ "variable " ++ show (varName v) ++ " appears in head, but not occur positively in body"
        forM_ subVars $ \v -> 
            when (v `notElem` bodyVars) $ do
                unexpected $ "variable " ++ show (varName v) ++ " appears in a negated subgoal, but not occur positively in body"
        return rule
    where
        headVars, bodyVars, subVars :: [Var]
        headVars = [ v | Var v <- atomArgs head ]
        bodyVars = concatMap posVars body
        subVars  = concatMap negVars body

        posVars, negVars :: Pat -> [Var]
        posVars (Pat atom) = [ v | Var v <- atomArgs atom ]
        posVars (Not _) = []
        negVars (Not atom) = [ v | Var v <- atomArgs atom ]
        negVars (Pat _) = []

statement :: P (Either Fact Rule)
statement = try (Left <$> fact)
            <|> (Right <$> rule) 
lineSep :: P ()
lineSep = spaced period <?> "."

statements :: P ([Fact],[Rule])
statements = do 
    spaces
    result <- partitionEithers <$> statement `sepEndBy` lineSep
    eof
    return result

initialEnv :: Env
initialEnv = Env { envNextFree = 0, envConMap = M.empty } 


class Pretty p where
    doc :: p -> PP.Doc

instance Pretty Con where
    doc = PP.text . conName 

instance Pretty Var where
    doc = PP.text . varName

instance Pretty Term where
    doc = eitherTerm doc doc 

instance Pretty a => Pretty (Atom a) where
    doc (Atom p b) = doc p <> PP.parens (PP.hsep $ PP.punctuate PP.comma (doc <$> b))

instance Pretty Pat where
    doc (Pat p) = doc p 
    doc (Not p) = PP.text "\\+" <+> doc p

instance Pretty Rule where
    doc (Rule h b) = 
        doc h <+> PP.text ":-" <+> (PP.hsep $ PP.punctuate PP.comma (doc <$> b))
        
instance Pretty [Rule] where
    doc [] = PP.empty
    doc (a:as) = doc a <> PP.text "." $$ doc as

instance Pretty [Fact] where
    doc [] = PP.empty
    doc (a:as) = doc a <> PP.text "." $$ doc as

instance Pretty ([Fact],[Rule]) where
    doc (x,y) = doc x $$ doc y

type Subst = [(Var,Con)]

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

run :: String -> Either ParseError ([Fact],[Rule])
run = runParser statements initialEnv "-" . T.pack

db = either (error . show) id $ run "man(socrates).mortal(X) :- man(X)"
rules = snd db
facts = fst db


