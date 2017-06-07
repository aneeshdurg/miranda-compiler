module Miranda.Compile where

import Miranda.Core
import qualified Data.HashMap.Strict as H 
import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

compile :: [Exp] -> ComState Exp
compile xs = do compileDefs xs
                compileProg

compileProg :: ComState Exp
compileProg = 
  do p <- getProg
     comProgH p
  where 
    --comProgH x@(Variable _) = x --TODO: replace this
                                --with lookups later
    comProgH (List xs) = do xs' <- mapM comProgH xs
                            return $ List xs

    comProgH (Tuple x y) = do x' <- comProgH x
                              y' <- comProgH y
                              return $ Tuple x' y'
    comProgH (App f xs) = do f' <- comProgH f
                             xs' <- mapM comProgH xs
                             case xs' of
                               [] -> return f'
                               _  -> return $ App f' xs'
    comProgH (Lambda x e) = do e' <- deltaLambda (comProgH e)
                               case x of
                                 Void -> return e'
                                 _    -> return $ Lambda x e'
    comProgH (Let (x, y) e) = do y' <- comProgH y
                                 e' <- comProgH e
                                 let l = Lambda x e'
                                 return $ App l [y']
    comProgH (Letrec xs e) = let l  = lambdaFlesh (map fst xs)
                             in  do e' <- comProgH e
                                    return $ App (l e') (map snd xs)
    comProgH (FatBar x y) = do x' <- comProgH x
                               inL <- inLambda
                               if inL
                                 then do y' <- comProgH y
                                         return $ FatBar x' y'
                                 else return x'
    comProgH x = return x

getProg :: ComState Exp
getProg = do env <- get
             case H.lookup "main" env of
               Just m -> return m
               _      -> throwError $ Err

inLambda :: ComState Bool
inLambda = do env <- get
              case H.lookup "__lambda_counter__" env of 
                Just (Constant (Number 0)) -> return $ False
                Just (Constant (Number n)) -> return $ True
                _                          -> return $ False

deltaLambda :: ComState Exp -> ComState Exp
deltaLambda e = do env <- get
                   let x = case H.lookup "__lambda_counter__" env of
                             Just (Constant (Number n)) -> n
                             _                          -> 0
                   modify $ H.insert "__lambda_counter__" (Constant (Number (x+1)))
                   put env
                   e

compileDefs :: [Exp] -> ComState ()
compileDefs [] = return ()
                                

compileDefs ((DefFun name (d:def)):rest) = 
  do env <- get
     if H.member name env
       then throwError Err
       else do modify $ H.insert name $ makeLambda (d:def)
               compileDefs rest

compileDefs _ = throwError $ Err

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

