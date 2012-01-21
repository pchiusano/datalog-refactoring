
module REPL where

import System.Console.Haskeline
import Katalog
import Control.Monad.Trans
import Control.Monad.State hiding (State)
import Data.Text as T
import Text.Parsec.Prim (runParser, getState)
import Data.Monoid
import Control.Applicative
import Backend
import PrettyPrint
import Parser

type ReplS = (DB, Env)

commands :: [(String, StateT ReplS (InputT IO) ())]
commands = [("facts", ac fst),
            ("rules", ac snd),
            ("edb", ac $ uncurry seminaive)]

ac f = get >>= lift . outputStrLn . show . doc . f . db . fst

repl :: IO ()
repl = runInputT defaultSettings $ evalStateT loop (mempty, initialEnv)
 where
   loop = do
     minput <- lift $ getInputLine "% "
     case minput of
       Nothing -> return ()
       Just ":q" -> return ()
       Just (':' : c) -> (maybe (unrecognized c) id $ lookup c commands) >> loop
       Just input -> do
         modify (\(DB _ dl, ps) -> let (nd, ns) = runP input ps in (DB False (dl `combine` nd), ns))
         loop
   unrecognized c = lift $ outputStrLn ("Unrecognized command " ++ c)
   runP s u = 
     either (error . show) id $
       runParser (statements >>= \x -> ((,) x) <$> getState) u "-" $ T.pack s
