

module Test where

import Control.Applicative
import Control.Monad

-- | One or none.
optionalA :: Alternative f => f a -> f (Maybe a)
optionalA v = (Just <$> v) <|> pure Nothing


maybe_mzero :: Maybe String
maybe_mzero = mzero

list_mzero :: [Int]
list_mzero = mzero

list_mplus01 :: [Int]
list_mplus01 = [1] `mplus` [2,3,4]

-- | Applicative cons.
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) p1 p2 = (:) <$> p1 <*> p2


   
count :: Applicative f => Int -> f a -> f [a]
count i p | i <= 0    = pure []
          | otherwise = p <:> count (i-1) p 

list_count01 :: [[Int]]
list_count01 = count 10 []

list_count02 :: [[Int]]          
list_count02 = count 10 [1]

list_count03 :: [[Int]]          
list_count03 = count 3 [1, 2]


countAc :: Applicative f => Int -> f a -> f [a]
countAc i p = countAcHelper i p (pure [])

countAcHelper :: Applicative f => Int -> f a -> f [a] -> f [a]
countAcHelper i p ac 
    | i <= 0    = ac
    | otherwise = countAcHelper (i-1) p (p <:> ac)

list_countAc01 :: [[Int]]
list_countAc01 = countAc 10 []

list_countAc02 :: [[Int]]          
list_countAc02 = countAc 10 [1]

list_countAc03 :: [[Int]]          
list_countAc03 = countAc 3 [1, 2]
