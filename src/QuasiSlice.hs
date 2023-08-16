{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module QuasiSlice   (IndexRange(..),
                    QuasiSlice(..),
                    evalSlice,
                    parseSlice)
where

import Text.ParserCombinators.Parsec
import Data.Typeable
import Data.Data

-- | Type which allows you to provide only a single index or a range of indicies.
data IndexRange = I Integer | R Integer Integer deriving (Show, Eq)

data QuasiSlice =
              NoIndexEx
           |  IndexEx Integer
           |  NegIndexEx Integer
           |  AntiIndexEx Bool String
           |  SliceEx QuasiSlice QuasiSlice
           -- |  AntiSliceExpr String
           |  CommaEx QuasiSlice QuasiSlice
           -- |  AntiCommaExpr String
    deriving(Show, Typeable, Data)

evalBound :: Bool -> QuasiSlice -> Integer
evalBound False NoIndexEx      = 0
evalBound True  NoIndexEx      = -1
evalBound _     (IndexEx i)    = i
evalBound _     (NegIndexEx i) = -i 

evalSlice :: QuasiSlice -> [IndexRange]
evalSlice x = case x of
    NoIndexEx          -> [R 0 (-1)]
    IndexEx i          -> [I i]
    NegIndexEx i       -> [I (-i)]
    SliceEx l r        -> [R (evalBound False l) (evalBound True r)]
    CommaEx ex1 ex2    -> evalSlice ex1 ++ evalSlice ex2

------------ PARSER

lexeme p     = do{ x <- p; spaces; return x  }
symbol name  = lexeme (string name)

comma = do{ symbol ","; return $ CommaEx }

indiciesExpr :: CharParser st QuasiSlice
indiciesExpr = sliceIndex `chainl1` comma

number :: CharParser st QuasiSlice
number = do
  m <- optionMaybe $ symbol "-"
  ds <- many digit
  case (m, ds) of
    (Nothing, []) -> try antiIntExpr <|> pure NoIndexEx
    (Nothing, _)  -> pure $ IndexEx (read ds)
    (Just _, [])  -> try (fmap antiNeg antiIntExpr) <|> pure NoIndexEx
    (Just _, _)   -> pure $ IndexEx (negate $ read ds)
  where 
    antiNeg (AntiIndexEx _ x) = AntiIndexEx False x

{-    
    == []
    then try anti <|> Nothing
    else Just (read ds)
  pure $ case m of
    Nothing -> n
    _       -> fmap negate n
-}

sliceIndex :: CharParser st QuasiSlice
sliceIndex = lexeme $ do
  l <- number
  s <- optionMaybe $ symbol ":"
  r <- number
  case s of
    Nothing -> pure l
    Just _ -> pure $ SliceEx l r

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
-}

small   = lower <|> char '_'
large   = upper
idchar  = small <|> large <|> digit <|> char '\''

ident  :: CharParser s String
ident  =  do{ c <- small; cs <- many idchar; return (c:cs) }

-- To include variables in scope not just integers
antiIntExpr = lexeme $ do{ id <- ident; return $ AntiIndexEx True id }
--antiIntExpr  = lexeme $ do{ symbol "$"; id <- ident; return $ AntiIndexEx id }
--antiExpr     = lexeme $ do{ symbol "$"; id <- ident; return $ AntiExpr id }

---------------

parseSlice :: (Monad m, MonadFail m) => (String, Int, Int) -> String -> m QuasiSlice
parseSlice (file, line, col) s =
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