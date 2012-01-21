
module REPL where

import System.Console.Haskeline
import Katalog
import Control.Monad.Trans
import Control.Monad.State
import Data.Monoid
import Control.Applicative

commands :: [(String, StateT Datalog (InputT IO) ())]
commands = [("facts", ac fst),
            ("rules", ac snd),
            ("edb", ac $ uncurry naive)]

ac f = get >>= lift . outputStrLn . show . doc . f

repl :: IO ()
repl = runInputT defaultSettings $ evalStateT loop ([],[])
 where
   loop = do
     minput <- lift $ getInputLine "% "
     case minput of
       Nothing -> return ()
       Just ":q" -> return ()
       Just (':' : c) -> (maybe (unrecognized c) id $ lookup c commands) >> loop
       Just input -> do
         modify . either (error . show) (flip mappend) $ run input
         loop
   unrecognized c = lift $ outputStrLn ("Unrecognized command " ++ c)

