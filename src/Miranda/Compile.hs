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

fixM :: (Exp -> ComState Exp) -> Exp -> ComState Exp
fixM f x = do x' <- f x
              if x' == x
                then return x
                else fixM f x'

compileProg :: ComState Exp
compileProg = 
  do p <- getProg
     fixM comProgH p

comProgH :: Exp -> ComState Exp
comProgH (Variable x) = 
  do env <- get
     case H.lookup x env of 
       Just exp -> return exp
       _        -> return $ Variable x
         --throwError $ undefinedVar x

comProgH (List xs) = do xs' <- mapM comProgH xs
                        return $ List xs

comProgH (Tuple x y) = do x' <- comProgH x
                          y' <- comProgH y
                          return $ Tuple x' y'

comProgH (App f@(Variable _) xs) = do f' <- comProgH f
                                      xs' <- mapM comProgH xs
                                      case xs' of
                                        [] -> return f'
                                        _  -> return $ App f' xs'
comProgH (App f@(Lambda _ _) xs) = do f' <- comProgH f
                                      xs' <- mapM comProgH xs
                                      case xs' of
                                        [] -> return f'
                                        _  -> return $ App f' xs'
                                              --match f' xs'

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
                           if inL || True
                             then do y' <- comProgH y
                                     return $ FatBar x' y'
                             else return x'

comProgH (If cond tb fb) = do cond' <- comProgH cond
                              tb'   <- comProgH tb
                              fb'   <- comProgH fb
                              return $ If cond' tb' fb'

comProgH x = return x
  --throwError $ Err $ "I don't know how to compile '"++(show x)++"'!"

match :: Exp -> [Exp] -> ComState Exp
match (Lambda (PVariable v) e) (x:xs) = 
  do env <- get
     modify (H.insert v x) 
     e' <- comProgH e
     put env
     match e' xs

match (Lambda (PConstant c) e) (x:xs) = 
  -- if c matches c, then e otherwise fail?
  return $ Constant (Number 1) --placeholder
match e [] = return $ e
match _ xs = throwError $ Err "Too many arguments!"

getProg :: ComState Exp
getProg = do env <- get
             case H.lookup "main" env of
               Just m -> return m
               _      -> throwError $ Err "No main function!"

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
       then throwError $ Err "Already defined function!"
       else do modify $ H.insert name $ makeLambda (d:def)
               compileDefs rest

compileDefs x = throwError $ Err $ "Cannot compile argument " ++ show x

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

