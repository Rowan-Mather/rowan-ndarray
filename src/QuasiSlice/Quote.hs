{-# LANGUAGE QuasiQuotes #-}

module QuasiSlice.Quote (q) where

--import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Data.Typeable
--import Language.Haskell.TH.Syntax(liftData)

import QuasiSlice

extQ :: ( Typeable a, Typeable b) => (a -> r) -> (b -> r) -> a -> r
extQ f g a = maybe (f a) g (cast a)

q :: QuasiQuoter
q = QuasiQuoter {  quoteExp = quoteExprExp
--                 , quotePat = quoteExprPat
                      -- with ghc >= 7.4, you could also
                      -- define quoteType and quoteDec for
                      -- quasiquotes in those places too
                    }
-------

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseSlice pos s
                      --dataToExpQ (\x -> Nothing) expr
                      --liftData expr
                      dataToExpQ (const Nothing `extQ` antiExprExp) expr

antiExprExp :: QuasiSlice -> Maybe (TH.Q TH.Exp)
antiExprExp (AntiIndexEx True v)  = Just $ TH.appE (TH.conE (TH.mkName "IndexEx"))
                                                   (TH.varE (TH.mkName v))
antiExprExp (AntiIndexEx False v) = Just $ TH.appE (TH.conE (TH.mkName "NegIndexEx"))
                                                   (TH.varE (TH.mkName v))
--antiExprExp  (AntiExpr v)     = Just $ TH.varE  (TH.mkName v)
antiExprExp _ = Nothing

-------
{-
quoteExprPat :: String -> TH.PatQ
quoteExprPat s =  do  loc <- TH.location
                      let pos =  (TH.loc_filename loc,
                                 fst (TH.loc_start loc),
                                 snd (TH.loc_start loc))
                      expr <- parseSlice pos s
                      dataToPatQ (const Nothing `extQ` antiExprPat) expr

antiExprPat :: QuasiSlice -> Maybe (TH.Q TH.Pat)
antiExprPat (AntiIndexEx v) = Just $ TH.conP  (TH.mkName "IndexEx")
                                                [TH.varP (TH.mkName v)]
--antiExprPat  (AntiExpr v)     = Just $ TH.varP  (TH.mkName v)
antiExprPat  _                = Nothing
-}