module Test where

{-cartProdN :: [(Int,Int)] -> [[Int]]
cartProdN = foldr
    (\(l,u) as ->
        [ x : a
        | x <- [l..u]
        , a <- as ])
    [[]]
-}

thing xs ys = sequence $ zipWith (\x y -> [x..y]) xs ys