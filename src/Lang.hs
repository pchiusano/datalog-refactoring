module Lang where

import Backend
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as M
import PrettyPrint

data E = 
    Ap E E
  | Builtin Name
  | Fn Name E
  | Ref Name
  | N Double

-- ap(id, fnid, argid).
-- builtin(id, nameid).
-- fn(id, varid, bodyid).
-- var(id, nameid).
-- num(id, v).
-- names(id, n).
-- types(id, typename). 


-- next fresh id, and the stack of variable bindings
type FS = (Int, [Map String Con], Map Con Name)
fs0 :: FS
fs0 = (6, [M.empty], M.empty) 

runFlatten :: E -> [Fact] 
runFlatten e = snd $ evalState (flatten e) fs0

-- return the con of the currently flattened expression, and other rows
flatten :: E -> State FS (Con, [Fact])
flatten (Ref s) = do 
  con <- getOrFresh s
  return (con, [Atom var [con]])
flatten (Ap f arg) = do 
  id1 <- freshId
  (e, db1) <- flatten f
  (e2,db2) <- flatten arg
  return $ let con = C id1 (show id1) in 
    (con, [Atom app [con, e, e2]] ++ db2 ++ db1 )
flatten (Builtin s) = do 
  con <- getOrFresh s 
  return (con, [Atom builtin [con]])
flatten (Fn name body) = do 
  id1 <- freshId
  _ <- modify (\(id,stack,names) -> (id,M.empty:stack,names))
  con <- return $ C id1 (show id1)
  v <- getOrFresh name
  (e,db) <- flatten body
  modify (\(id,h:stack,names) -> (id,stack,names)) 
  return (con, Atom fn [con, v, e] : db) 
flatten (N d) = do
  con <- getOrFresh (show d) -- todo: possibly add primitives
  return (con, [Atom num [con]])

--data US = US { 
--  cache :: Map Con E,
--  aps :: Map Con Fact,
--  builtins :: Map Con Fact,
--  fns :: Map Con Fact, 
--  vars :: Map Con Fact,
--  nums :: Map Con Fact,
--  names :: Map Con Fact,  
--  refs :: Map Con Fact 
--

unflatten :: Con -> [Fact] -> E
unflatten c db = go c (ustate db)
  where
    go :: Con -> US -> E
    go C 

app = C 0 "ap"
builtin = C 1 "builtin"
fn = C 2 "fn"
var = C 3 "var" 
num = C 4 "num"
name = C 5 "name"

freshId :: State FS Int
freshId = do 
  (nextId, stack, names) <- get
  _ <- put (nextId+1, stack, names)
  return nextId

getOrFresh :: Name -> State FS Con 
getOrFresh n = do 
  (_, b:_, _) <- get
  maybe (fresh n) return (M.lookup n b) 
  where
    fresh :: Name -> State FS Con
    fresh n = do 
      (nextId, h:t, names) <- get
      let con = C nextId n in do
        _ <- put $ (nextId+1, (M.insert n con h) : t, M.insert con n names) 
        return con 
 
