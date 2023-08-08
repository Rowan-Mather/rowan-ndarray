{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Typing where

import Type.Reflection

-- * Typing Shorthand 
-- | typeOf synonym.
ty :: Typeable a => a -> TypeRep a
ty = typeOf

-- | eqTypeRep synonym, returning Just HRefl in the case of type equality.
-- >>> case True =@= False of
-- >>>  Just HRefl -> putStrLn "Two Booleans will match"
-- >>>  Nothing    -> putStrLn "Mismatching types"
-- Two Booleans will match
(=@=) :: (Typeable a, Typeable b) => a -> b -> Maybe (a :~~: b)
(=@=) v u = eqTypeRep (ty v) (ty u)

(=@) :: Typeable a => a -> TypeRep b -> Maybe (a :~~: b)
(=@) x t = eqTypeRep (ty x) t

-- Helper asserting a type
(<-@) ::Typeable a => a -> TypeRep b -> b
(<-@) val t = case eqTypeRep t (ty val) of
  Just HRefl -> val
  _ -> error "Mismatching type."