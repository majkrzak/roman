module Roman.Model.SnakeSegment
  ( SnakeSegment(Head, Body, Tail)
  ) where

import Prelude (Show, Eq, Enum)

-- | Type of snake segment.
-- >    ~~~~~~~8<
-- > Tail|Body|Head
data SnakeSegment
  = Head
  | Body
  | Tail
  deriving(Show, Eq, Enum)
