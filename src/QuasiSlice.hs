{-# LANGUAGE DeriveDataTypeable #-}

module QuasiSlice   (Expr(..),
                    BinOp(..),
                    eval,
                    parseExpr)
where

import Indexing

import Data.Generics
import Text.ParserCombinators.Parsec

data Expr  =  
           |  IndexExpr (Maybe Integer)
           |  AntiIndexExpr String
           |  SliceExpr (Maybe Integer) (Maybe Integer)
           |  AntiSliceExpr String
           |  CommaExpr Expr Expr
           |  AntiCommaExpr String
    deriving(Show, Typeable, Data)

eval :: Expr -> [IndexRange]
eval x = case x of 
    IndexExpr Nothing           -> [R 0 (-1)]
    IndexExpr (Just i)          -> [I i]
    SliceExpr Nothing Nothing   -> [R 0 (-1)]
    SliceExpr Nothing (Just j)  -> [R 0 j]
    SliceExpr (Just i) Nothing  -> [R i (-1)]
    SliceExpr (Just i) (Just j) -> [R i j]
    CommaExpr ex1 ex2           -> eval ex1 ++ eval ex2


------------ PARSER

-- todo negative numbers

--small   = lower <|> char '_'
--large   = upper
--idchar  = small <|> large <|> digit <|> char '\''

lexeme p     = do{ x <- p; spaces; return x  }
symbol name  = lexeme (string name)
--parens p     = between (symbol "(") (symbol ")") p

expr :: CharParser st Expr
expr = sliceIndex `chainl1` comma

comma = do{ symbol ","; return $ CommaExpr }
--colon = do{ symbol ":"; return $ SliceExpr }

sliceIndex :: CharParser st Expr
sliceIndex = index <|> 
             sliceWhole <|> sliceLeft <|> sliceRight <|> sliceEmpty <|>
             try antiIntExpr <|> antiExpr

index :: CharParser st Expr
index = lexeme $ do{ ds <- many1 digit ; return $ IndexExpr (Just $ read ds) }
 
sliceWhole :: CharParser st Expr
sliceWhole = lexeme $ do{ d1s <- many1 digit ; symbol ":" ; d2s <- many1 digit ; return $ SliceExpr (Just $ read d1s) (Just $ read d2s) }

sliceLeft :: CharParser st Expr
sliceLeft = lexeme $ do{ ds <- many1 digit ; symbol ":" ; return $ SliceExpr (Just $ read ds) Nothing }

sliceRight :: CharParser st Expr
sliceRight = lexeme $ do{ symbol ":" ; ds <- many1 digit ; return $ SliceExpr Nothing (Just $ read ds) }

sliceEmpty :: CharParser st Expr
sliceEmpty = lexeme $ do{ symbol ":"; return $ SliceExpr Nothing Nothing }

ident  :: CharParser s String
ident  =  do{ c <- small; cs <- many idchar; return (c:cs) }

antiIntExpr  = lexeme $ do{ symbol "$int:"; id <- ident; return $ AntiIntExpr id }
antiExpr     = lexeme $ do{ symbol "$"; id <- ident; return $ AntiExpr id }

---------------

parseExpr :: (Monad m, MonadFail m) => (String, Int, Int) -> String -> m Expr
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
            e <- expr
            eof
            return e