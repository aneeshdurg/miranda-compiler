module Main where
import Miranda.Core
import Miranda.Parse
import Miranda.Compile

import Prelude hiding (lookup)
import System.IO (hFlush, hPutStr, hPutStrLn, hGetLine, stdin, stdout)
import System.Console.Readline
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Data.Functor.Identity
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.HashMap.Strict as H (empty)

import System.Environment (getArgs)

runParse = \x y z -> runIdentity $ runParserT x All y z

main = do a <- getArgs
          if length a == 0
            then startRepl
            else let name = head a 
                 in  do x <- readFile name 
                        case runParse exprP name x of
                          Left err -> print err
                          Right exp -> case runExcept $ runStateT (compile exp) H.empty of
                                         Left err -> print err
                                         Right (exp, env) -> do print exp
                                                                putStr "\n\n"
                                                                print env

startRepl = do x <- readline "Miranda> "
               case x of 
                 Just l -> do print $ runParse exprP "<stdin>" l 
                              startRepl
                 _      -> putStrLn "GOODBYE"
