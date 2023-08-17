{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module QuasiSlice   (QuasiSlice(..),
                    evalSlice,
                    parseExpr)
where

import Indexing

import Data.Generics
import Text.ParserCombinators.Parsec

data QuasiSlice =
              IndexExpr (Maybe Integer)
           -- |  AntiIndexExpr String
           |  SliceExpr (Maybe Integer) (Maybe Integer)
           -- |  AntiSliceExpr String
           |  CommaExpr QuasiSlice QuasiSlice
           -- |  AntiCommaExpr String
    deriving(Show, Typeable, Data)

evalSlice :: QuasiSlice -> [IndexRange]
evalSlice x = case x of
    IndexExpr Nothing           -> [R 0 (-1)]
    IndexExpr (Just i)          -> [I i]
    SliceExpr Nothing Nothing   -> [R 0 (-1)]
    SliceExpr Nothing (Just j)  -> [R 0 j]
    SliceExpr (Just i) Nothing  -> [R i (-1)]
    SliceExpr (Just i) (Just j) -> [R i j]
    CommaExpr ex1 ex2           -> evalSlice ex1 ++ evalSlice ex2

------------ PARSER

-- todo negative numbers

--small   = lower <|> char '_'
--large   = upper
--idchar  = small <|> large <|> digit <|> char '\''

lexeme p     = do{ x <- p; spaces; return x  }
symbol name  = lexeme (string name)
--parens p     = between (symbol "(") (symbol ")") p

comma = do{ symbol ","; return $ CommaExpr }

indiciesExpr :: CharParser st QuasiSlice
indiciesExpr = sliceIndex `chainl1` comma

sliceIndex :: CharParser st QuasiSlice
sliceIndex = lexeme $ do 
  d1s <- many digit
  s <- optionMaybe $ symbol ":"
  d2s <- many digit
  let l = if d1s == [] then Nothing else Just (read d1s)
  let r = if d2s == [] then Nothing else Just (read d2s) 
  case s of 
    Nothing -> return $ IndexExpr l
    Just _ -> return $ SliceExpr l r
{-
sliceIndex :: CharParser st QuasiSlice
sliceIndex = sliceExpr <|> indexExpr  
             
index :: CharParser st QuasiSlice
index = lexeme $ do
  ds <- many1 digit
  --let i = if ds == [] then Nothing else Just (read ds)
  return $ IndexExpr $ Just (read ds)

sliceExpr :: CharParser st QuasiSlice
sliceExpr = try $ lexeme $ do 
  d1s <- many digit
  symbol ":"
  d2s <- many digit
  let l = if d1s == [] then Nothing else Just (read d1s)
  let r = if d2s == [] then Nothing else Just (read d2s) 
  return $ SliceExpr l r
-}
{-
indexEmpty :: CharParser st QuasiSlice
indexEmpty = lexeme $ do{ return $ IndexExpr Nothing }

sliceLeft :: CharParser st QuasiSlice
sliceLeft = lexeme $ do{ ds <- many1 digit ; symbol ":" ; return $ SliceExpr (Just $ read ds) Nothing }

sliceRight :: CharParser st QuasiSlice
sliceRight = lexeme $ do{ symbol ":" ; ds <- many1 digit ; return $ SliceExpr Nothing (Just $ read ds) }

sliceEmpty :: CharParser st QuasiSlice
sliceEmpty = lexeme $ do{ symbol ":"; return $ SliceExpr Nothing Nothing }

--ident  :: CharParser s String
--ident  =  do{ c <- small; cs <- many idchar; return (c:cs) }
-}
-- To include variables in scope not just integers
--antiIntExpr  = lexeme $ do{ symbol "$int:"; id <- ident; return $ AntiIntExpr id }
--antiExpr     = lexeme $ do{ symbol "$"; id <- ident; return $ AntiExpr id }

---------------

parseExpr :: (Monad m, MonadFail m) => (String, Int, Int) -> String -> m QuasiSlice
parseExpr (file, line, col) s =
    case runParser p () "" s of
      Left err  -> fail $ show err
      Right e   -> return e
  where
    p = do  pos <- getPosition
            setPosition $
              (flip setSourceName) file $
              (flip setSourceLine) line $
              (flip setSourceColumn) col $
              pos
            spaces
            e <- indiciesExpr
            eof
            return e