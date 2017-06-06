module Miranda.Compile where

import Miranda.Core
import qualified Data.HashMap.Strict as H 
import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

compile :: [Exp] -> ComState Exp
compile = compileH 

compileH :: [Exp] -> ComState Exp
compileH [] = do env <- get
                 case H.lookup "main" env of
                   Just m -> return m
                   _      -> throwError $ Err
                 

compileH ((DefFun name (d:def)):rest) = 
  do env <- get
     if H.member name env
       then throwError Err
       else do modify $ H.insert name $ makeLambda (d:def)
               compileH rest

compileH _ = throwError $ Err

makeLambda :: [([Pattern], Exp)] -> Exp
makeLambda (d:def) = let (l, vs) = (lambdaSkeleton $ (length . fst) d) 
                         lams = map (appLambdaFromDef vs) (d:def)
                     in  l $ makeFatBar lams

makeFatBar :: [Exp] -> Exp
makeFatBar [] = ERROR
makeFatBar (x:xs) = FatBar x $ makeFatBar xs

lambdaSkeleton :: Int -> ((Exp -> Exp), [String])
lambdaSkeleton 0 = (Lambda Void, [])
lambdaSkeleton 1 = (Lambda (PVariable "_lambdaVar1"), ["_lambdaVar1"])
lambdaSkeleton n = let (l, vs) = lambdaSkeleton (n-1)
                       name    = "_lambdaVar" ++ show n
                   in  ((\x -> Lambda (PVariable name) (l x)), name:vs)


lambdaFlesh :: [Pattern] -> (Exp -> Exp)
lambdaFlesh [] = Lambda Void
lambdaFlesh (p:[]) = Lambda p
lambdaFlesh (p:ps) = (\x -> Lambda p $ (lambdaFlesh ps) x)

appLambdaFromDef :: [String] -> ([Pattern], Exp) -> Exp
appLambdaFromDef names (ps, e) = let l = lambdaFlesh ps
                                     lam = l e
                                 in  App lam $ map Variable names

