
module REPL where

import System.Console.Haskeline
import Katalog
import Control.Monad.Trans
import Control.Monad.State hiding (State)
import Data.Text as T
import Text.Parsec.Prim
import Data.Monoid
import Control.Applicative
import Backend

type ReplS = (Datalog, Env)

commands :: [(String, StateT ReplS (InputT IO) ())]
commands = [("facts", ac fst),
            ("rules", ac snd),
            ("edb", ac $ uncurry seminaive)]

ac f = get >>= lift . outputStrLn . show . doc . f . fst

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
         modify (\(dl, ps) -> let (nd, ns) = runP input ps in (dl `combine` nd, ns))
         loop
   unrecognized c = lift $ outputStrLn ("Unrecognized command " ++ c)
   runP s u = 
     either (error . show) id $
       runParser (statements >>= \x -> ((,) x) <$> getState) u "-" $ T.pack s
