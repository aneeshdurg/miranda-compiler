module Miranda.Core where

data PrimType = Number Int 
              | Character Char
              deriving Show

data Exp = Constant PrimType
         | Variable [Char]
         | App Exp [Exp]
         | Lambda Pattern Exp
         | Let (Pattern, Exp) Exp
         | Letrec [(Pattern, Exp)] Exp
         | FatBar Exp Exp
         | Case Exp [(Pattern, Exp)]
         | DefFun [Char] Exp 
         deriving Show

data Pattern = PConstant PrimType
             | PVariable [Char]
             | Construct [Char] [Pattern]
             deriving Show
             

