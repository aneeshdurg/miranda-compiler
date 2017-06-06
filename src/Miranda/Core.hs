module Miranda.Core where

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

data PrimType = Number Int 
              | Character Char
              | Boolean Bool
              deriving Show

data Exp = Constant PrimType
         | Variable [Char]
         | List [Exp]
         | App Exp [Exp]
         | Lambda Pattern Exp
         | Let (Pattern, Exp) Exp
         | Letrec [(Pattern, Exp)] Exp
         | FatBar Exp Exp
         | Case Exp [(Pattern, Exp)]
         | DefFun [Char] [([Pattern], Exp)]
         | ERROR
         deriving Show

data Pattern = PConstant PrimType
             | PVariable [Char]
             | Construct [Char] [Pattern]
             | Void
             deriving Show
             
data Diagnostic = Err deriving Show 
-- ### Evaluation monad

-- `StateT` is the monad transformer version of `State`. You do not need to
-- understand monad transformers! Simply read the following declaration as:
-- `EvalState` is a state encapsulating the evaluation result of type `a` and
-- the environment of type `Env`, except when a `Diagnostic` is thown along the
-- evaluation
type ComState a = StateT (H.HashMap String Exp) (Except Diagnostic) a

