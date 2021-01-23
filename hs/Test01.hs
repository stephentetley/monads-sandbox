

module Test where

import Control.Applicative

-- | One or none.
optionalA :: Alternative f => f a -> f (Maybe a)
optionalA v = (Just <$> v) <|> pure Nothing
