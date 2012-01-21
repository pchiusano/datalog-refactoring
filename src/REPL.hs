
module REPL where

import System.Console.Haskeline
import Katalog
import Control.Monad.Trans
import Control.Monad.State
import Data.Monoid

repl :: IO ()
repl = runInputT defaultSettings $ evalStateT loop ([],[])
 where
   loop = do
     minput <- lift $ getInputLine "% "
     case minput of
       Nothing -> return ()
       Just ":q" -> return ()
       Just input -> do
         modify . either (error . show) (flip mappend) $ run input
         loop

