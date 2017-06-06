module Miranda.Parse where

import Miranda.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Control.Monad

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as H

data ParserFlag = All | NoInfix | NoApp | None deriving (Eq, Show)

type Parser = ParsecT String ParserFlag Identity

reservedWords = S.fromList ["True", "False", "in", "let", "letrec"]

subExps = H.fromList [ (show All, rawExprP)
                     , (show NoInfix, noInfixOpP)
                     , (show NoApp, noAppExpP)
                     , (show None, noLeftRecursiveOps) 
                     ]

subExpsLookup x = case H.lookup (show x) subExps of
                    Just e -> e
                    _      -> rawExprP

addFlag :: ParserFlag -> ParserFlag -> ParserFlag
addFlag All x = x
addFlag x All = x
addFlag _ _   = None

digitP :: Parser Char
digitP = oneOf ['0'..'9']

digitsP :: Parser String
digitsP = many1 digitP

anyWSP :: Parser String
anyWSP = many1 $ oneOf " \t\n" 

maybeWSP :: Parser String
maybeWSP = many $ oneOf " \t\n" 

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \t"

geq1line :: Parser String
geq1line = maybeSpaceP >> char '\n' >> maybeWSP

idP :: Parser String
idP = liftM2 (:) identFirst (many identRest)
  where identFirst = oneOf $ ['a'..'z'] ++ ['A'..'Z']
        identRest  = identFirst <|> digitP

numP :: Parser Exp
numP = Constant . Number . read <$> digitsP 

charP :: Parser Exp
charP = do char '\''
           c <- anyChar
           char '\''
           return $ Constant $ Character c

boolP :: Parser Exp
boolP = Constant . Boolean . read <$> strBoolP
        where strBoolP = (string "True") <|> (string "False")

           
constantP :: Parser Exp
constantP = numP <|> charP <|> boolP

varP :: Parser Exp 
varP = try $ do s <- idP
                if S.member s reservedWords
                  then fail "Reserved keyword!"
                  else return $ Variable s

letBinding :: Parser (Pattern, Exp)
letBinding = try $ do pattern <- patP
                      maybeWSP >> char '=' >> maybeWSP
                      exp <- rawExprP
                      return $ (pattern, exp)

letP :: Parser Exp 
letP = try $ do string "let"
                anyWSP
                (pattern, subs) <- letBinding
                maybeWSP
                string "in"
                anyWSP
                expr <- rawExprP
                return $ Let (pattern, subs) expr

letrecP :: Parser Exp
letrecP = try $ do string "letrec"
                   anyWSP
                   subs <-letBinding `sepEndBy` geq1line 
                   maybeWSP
                   string "in"
                   anyWSP
                   expr <- rawExprP
                   return $ Letrec subs expr 
          
patP :: Parser Pattern 
patP = getCons <|> getC <|> getV <|> parensPat
       where parensPat = do char '('
                            maybeWSP
                            p <- patP
                            maybeWSP
                            char ')'
                            return p
             getC = do Constant c <- constantP
                       return $ PConstant c
             getV = do Variable v <- varP
                       return $ PVariable v
             getCons = do c <- oneOf ['A'..'Z']
                          Variable v <- varP
                          many $ char ' '
                          patts <- patP `sepEndBy` (char ' ')
                          return $ Construct (c:v) patts
tupleP :: Parser Exp
tupleP = try $ do char '('
                  maybeWSP
                  e <- rawExprP
                  maybeWSP >> char ',' >> maybeWSP
                  e' <- rawExprP
                  maybeWSP
                  char ')'
                  return $ Tuple e e'

parensP :: Parser Exp
parensP = do char '('
             maybeWSP
             e <- rawExprP
             maybeWSP
             char ')'
             return e

listP :: Parser Exp
listP = do char '['
           maybeWSP
           e <- rawExprP `sepEndBy` (maybeWSP >> char ',' >> maybeWSP)
           maybeWSP
           char ']'
           return $ List e

deffunP :: Parser Exp
deffunP = try $ do Variable v <- varP
                   maybeSpaceP
                   p <- many patP
                   maybeWSP
                   char '='
                   maybeWSP
                   e <- rawExprP
                   --maybeWSP
                   geq1line
                   cases <-  (funGen v (length p)) `sepEndBy` geq1line
                   return $ DefFun v ((p, e):cases)
          where funGen name len = 
                  try $ do string name
                           maybeSpaceP
                           p <- many patP
                           maybeWSP 
                           char '='
                           maybeWSP
                           if (length p) /= len
                             then fail $ "Function "++name++" has variable number of arguments"
                             else do e <- rawExprP
                                     return (p, e)


appfunP :: Parser Exp
appfunP = try $ do flag <- getState
                   modifyState (addFlag NoApp)
                   flag' <- getState 
                   let subExpP = subExpsLookup flag'
                   f <- subExpP 
                   spaceP
                   x <- subExpP `sepEndBy` spaceP
                   putState flag
                   case x of
                     [] -> return f
                     _  -> return $ App f x

infixOpP :: Parser Exp
infixOpP = try $ do flag <- getState
                    modifyState (addFlag NoInfix)
                    flag' <- getState
                    let subExpP = subExpsLookup flag'
                    e <- subExpP          
                    maybeWSP
                    op <- getInfixOp          
                    maybeWSP
                    e' <- subExpP 
                    putState flag
                    return $ App (Variable op) [e, e']
                   
getInfixOp :: Parser String                   
getInfixOp = oneCharBuiltIns <|> builtIns -- <|> TODO: User defined infix ops 
             where oneCharBuiltIns = do c <- oneOf "+-/*<>"
                                        return $ c:[]
                   builtIns = string "=="
                           <|> string "<="
                           <|> string ">="
                           <|> string "&&"
                           <|> string "||"
              

noAppExpP :: Parser Exp
noAppExpP = infixOpP <|> reservedNamesP <|> miscP      

noInfixOpP :: Parser Exp
noInfixOpP = reservedNamesP <|> appfunP <|> miscP

noLeftRecursiveOps :: Parser Exp
noLeftRecursiveOps = reservedNamesP <|> miscP

reservedNamesP :: Parser Exp
reservedNamesP = letP
              <|> letrecP
              <|> constantP
 
miscP :: Parser Exp
miscP = tupleP
     <|> parensP
     <|> listP
     <|> deffunP
     <|> varP

rawExprP :: Parser Exp
rawExprP = infixOpP <|> reservedNamesP <|> appfunP <|> miscP

emptyLine :: Parser String
emptyLine = maybeSpaceP >> char '\n' >> maybeSpaceP

exprP :: Parser [Exp]
exprP = many $ do e <- rawExprP
                  many emptyLine
                  return e

--TODO
--caseP
