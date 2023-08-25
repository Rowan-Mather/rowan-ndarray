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

-- | Type which allows you to provide only a single index or a range of indices.
data IndexRange = I Integer | R Integer Integer deriving (Show, Eq)

-- QuasiQuoted slices are converted to this to be evaluated.
data QuasiSlice =
              NoIndexEx
           |  IndexEx Integer
           |  NegIndexEx Integer
           |  AntiIndexEx Bool String
           |  SliceEx QuasiSlice QuasiSlice
           |  CommaEx QuasiSlice QuasiSlice
    deriving(Show, Typeable, Data)

-- Checks for the prescence of a value in a range e.g. ():4)
evalBound :: Bool -> QuasiSlice -> Integer
evalBound False NoIndexEx      = 0
evalBound True  NoIndexEx      = -1
evalBound _     (IndexEx i)    = i
evalBound _     (NegIndexEx i) = -i 

-- Converts the Quasi slice to an IndexRange which can be operated on as usual in Indexing.
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

indicesExpr :: CharParser st QuasiSlice
indicesExpr = sliceIndex `chainl1` comma

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

sliceIndex :: CharParser st QuasiSlice
sliceIndex = lexeme $ do
  l <- number
  s <- optionMaybe $ symbol ":"
  r <- number
  case s of
    Nothing -> pure l
    Just _ -> pure $ SliceEx l r

small   = lower <|> char '_'
large   = upper
idchar  = small <|> large <|> digit <|> char '\''

ident  :: CharParser s String
ident  =  do{ c <- small; cs <- many idchar; return (c:cs) }

-- To include variables in scope, not just integers
antiIntExpr = lexeme $ do{ id <- ident; return $ AntiIndexEx True id }
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
            e <- indicesExpr
            eof
            return e