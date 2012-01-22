{-# LANGUAGE Rank2Types #-} 
module REPL where

import System.Console.Haskeline
import Katalog
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.Text as T
import Text.Parsec.Prim (runParser, getState, setState)
import Control.Applicative
import Backend
import PrettyPrint
import Parser
import Data.IORef


type ReplS = Env

showDoc :: Pretty p => p -> String
showDoc = show . doc

commands2 :: Backend f => [(String, f String)]
commands2 = [
  ("facts", liftM showDoc facts), 
  ("rules", liftM showDoc rules)]

runCommands :: Backend f => (forall a . f a -> IO a) -> [(String, IO ())]
runCommands run = map (\(a,b) -> (a, run b >>= putStrLn)) commands2  

ac f = get >>= lift . outputStrLn . show . doc . f . db . fst

repl :: Backend f => LowerIO f -> IO ()
repl io = let 
   table = runCommands (trans io)

   parser :: P (Datalog, Env)
   parser = do 
     db <- statements
     env2 <- getState
     return (db, env2)

   handleResult :: (Datalog, Env) -> IO Env 
   handleResult (db, env) = trans io $ declare db >> return env

   parse :: Env -> String -> IO Env
   parse env line = either (error . show) handleResult $
     runParser parser env "<console>" (T.pack line)

   loop :: [(String, IO ())] 
        -> (Env -> String -> IO Env) 
        -> Env 
        -> InputT IO Env
   loop commands stmt env = do
     minput <- getInputLine "% "
     case minput of
       Nothing -> return env
       Just ":q" -> return env
       Just (':' : t) -> 
         maybe (unrecognized t) lift (lookup t commands) >> 
         loop commands stmt env
       Just input -> lift (stmt env input) >>= \env2 -> loop commands stmt env2 

   unrecognized c = outputStrLn ("Unrecognized command " ++ c)

   in 
   runInputT defaultSettings $ loop table parse initialEnv >> return ()

data NT m n = NT { trans :: forall a . m a -> n a }

type LowerIO m = NT m IO

stateLowerIO :: s -> IO (LowerIO (State s))
stateLowerIO s = do
  ref <- newIORef s
  return $ NT (\ma -> do 
    si <- readIORef ref
    (a,s2) <- return $ runState ma si
    _ <- writeIORef ref s2
    return a)
