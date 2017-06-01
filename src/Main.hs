module Main where
import Miranda.Core
import Miranda.Parse

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import System.Console.Readline
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Strict as H (empty)

main = putStrLn "hi"
