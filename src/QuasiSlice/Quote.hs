{-# LANGUAGE QuasiQuotes #-}

module QuasiSlice.Quote (expr) where

import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax(liftData)

import QuasiSlice

quoteExprExp :: String -> TH.ExpQ
--quoteExprPat :: String -> TH.PatQ

expr :: QuasiQuoter
expr = QuasiQuoter { quoteExp = quoteExprExp
--                   ,    quotePat = quoteExprPat
                      -- with ghc >= 7.4, you could also
                      -- define quoteType and quoteDec for
                      -- quasiquotes in those places too
                    }
-------
quoteExprExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseExpr pos s
                      --dataToExpQ (\x -> Nothing) expr
                      liftData expr
                      --dataToExpQ (const Nothing `extQ` antiExprExp) expr
{-
antiExprExp :: QuasiSlice -> Maybe (TH.Q TH.Exp)
antiExprExp  (AntiIntExpr v)  = Just $ TH.appE  (TH.conE (TH.mkName "IntExpr"))
                                                (TH.varE (TH.mkName v))
antiExprExp  (AntiExpr v)     = Just $ TH.varE  (TH.mkName v)
antiExprExp  _                = Nothing

-------
quoteExprPat s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseExpr pos s
                      dataToPatQ (const Nothing `extQ` antiExprPat) expr

--antiExprPat :: QuasiSlice -> Maybe (TH.Q TH.Pat)
--antiExprPat  (AntiIntExpr v)  = Just $ TH.conP  (TH.mkName "IntExpr")
                                                [TH.varP (TH.mkName v)]
--antiExprPat  (AntiExpr v)     = Just $ TH.varP  (TH.mkName v)
--antiExprPat  _                = Nothing
-}