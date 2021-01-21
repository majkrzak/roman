module Roman.Model.Direction
  ( Direction(North, East, South, West)
  ) where

import Prelude (Show, Eq, Enum, Bounded)

-- | Type of movement direction.
--   Represented with four cardinal directions,
--   where North is pointing upwards the screen,
--   as presented on figure bellow:
-- >        North
-- >          ^
-- >          |
-- > West < - o - > East
-- >          :
-- >          v
-- >        South
data Direction
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Enum, Bounded)
