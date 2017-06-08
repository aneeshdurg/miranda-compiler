{-# LANGUAGE DeriveGeneric #-}
module Miranda.Core where

import Generics.Deriving.Base (Generic)
import Generics.Deriving.Show (GShow, gshow)

import Data.HashMap.Strict as H (HashMap, insert, lookup, empty, fromList)
import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

data PrimType = Number Int 
              | Character Char
              | Boolean Bool
              deriving (Generic)

instance Show PrimType where
  show (Number n) = show n
  show (Character c) = show c
  show (Boolean b) = show b

data Exp = Constant PrimType
         | Variable [Char]
         | List [Exp]
         | Tuple Exp Exp
         | App Exp [Exp]
         | Lambda Pattern Exp
         | Let (Pattern, Exp) Exp
         | Letrec [(Pattern, Exp)] Exp
         | FatBar Exp Exp
         | If Exp Exp Exp
-- ?     | Construct [Char] [(Exp] 
-- ?     | TypeDef [Char] Int [Char]
         | DefFun [Char] [([Pattern], Exp)]
         | ERROR
         deriving (Generic)

instance GShow Exp
instance GShow PrimType
instance GShow Pattern
instance Show Exp where
  show (Constant x) = show x
  show (Variable x) = x
  show (List xs) = show xs
  show (Tuple x y) = show (x,y)
  show (App x y) = "(" ++ (show x)++" (applied to) "++(show y)++")"
  show (Lambda p e) = "\\" ++ (show p) ++ " . " ++ show e 
  show (Let (p, e) e') = let s_p = show p
                             s_e = show e
                             s_e' = show e'
                         in  "let "++s_p++" = "++s_e++" in "++s_e'
  show (Letrec ps e) = "let "++(show ps)++" in "++show e
  show (FatBar x y) = (show x) ++ "[]" ++ show y
  show (If c t f) = "If ("++(show c)++"){"++(show t)++"}else{"++(show f)++"}"
  show (DefFun name def) = "DEFINE: "++name++" "++show def
  show x = gshow x

data Pattern = PConstant PrimType
             | PVariable [Char]
             | PConstruct [Char] [Pattern]
             | Void
             deriving (Show, Generic)
             
data Diagnostic = Err [Char] deriving Show 
-- ### Evaluation monad

-- `StateT` is the monad transformer version of `State`. You do not need to
-- understand monad transformers! Simply read the following declaration as:
-- `EvalState` is a state encapsulating the evaluation result of type `a` and
-- the environment of type `Env`, except when a `Diagnostic` is thown along the
-- evaluation
type ComState a = StateT (H.HashMap String Exp) (Except Diagnostic) a

