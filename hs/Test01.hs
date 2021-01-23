

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

