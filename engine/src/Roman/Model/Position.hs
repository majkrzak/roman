module Roman.Model.Position
  ( Position(Position)
  ) where

import Prelude (Int, Show, Eq)

-- | Type of matrix coordinates.
--   `Position (i,j)` represents:
-- >   _ _ _ j _ _ 
-- >  |      .
-- >  |      .
-- >  i .  . x
-- >  |
--   Lowest possible index is 1 highest SIZE.
newtype Position
  = Position (Int, Int)
  deriving (Show, Eq)
