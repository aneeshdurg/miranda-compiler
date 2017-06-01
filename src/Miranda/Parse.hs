module Miranda.Parse where

import Miranda.Core

import Data.Functor.Identity
import Text.ParserCombinators.Parsec hiding (Parser, State)
import Text.Parsec.Prim hiding (State, try)
import Control.Monad

type Parser = ParsecT String () Identity

parseWith :: Parser a -> String -> Either ParseError a
parseWith p = parse p ""

digitP :: Parser Char
digitP = oneOf ['0'..'9']

digitsP :: Parser String
digitsP = many1 digitP

maybeSpaceP :: Parser String
maybeSpaceP = many $ oneOf " \n\t"

spaceP :: Parser String
spaceP = many1 $ oneOf " \n\t"

idP :: Parser String
idP = liftM2 (:) identFirst (many identRest)
  where identFirst = oneOf $ ['a'..'z'] ++ ['A'..'Z']
        identRest  = identFirst <|> digitP

numP :: Parser Exp
numP = Constant . Number . read <$> digitsP 

constantP :: Parser Exp
constantP = numP -- <|> charP

varP :: Parser Exp 
varP = Variable <$> idP

letBinding :: Parser (Pattern, Exp)
letBinding = try $ do pattern <- patP
                      maybeSpaceP >> char '=' >> maybeSpaceP
                      exp <- rawExprP
                      return $ (pattern, exp)

letP :: Parser Exp 
letP = try $ do string "let"
                spaceP
                (pattern, subs) <- letBinding
                spaceP
                string "in"
                spaceP
                expr <- rawExprP
                return $ Let (pattern, subs) expr 

letrecP :: Parser Exp
letrecP = do string "letrec"
             spaceP
             subs <-letBinding `sepEndBy` maybeSpaceP 
             maybeSpaceP
             string "in"
             spaceP
             expr <- rawExprP
             return $ Letrec subs expr 
          
patP :: Parser Pattern 
patP = getCons <|> getC <|> getV <|> parensPat
       where parensPat = do char '('
                            p <- patP
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

parensP :: Parser Exp
parensP = do char '('
             e <- rawExprP
             char ')'
             return e

caseP
deffunP

rawExprP :: Parser Exp
rawExprP = letP
        <|> letrecP
        <|> constantP
        <|> parensP
        <|> varP

