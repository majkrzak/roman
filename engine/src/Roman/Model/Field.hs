module Roman.Model.Field
  ( Field(Field)
  , Snake(Snake, NoSnake)
  , Fruit(Fruit, NoFruit)
  ) where

import Prelude (Show, Eq)

import Roman.Model.Direction
import Roman.Model.SnakeSegment

-- | Type of bord fields.
--   Contains information about presence
--   of the Snake and the Fruit.
newtype Field
  = Field (Snake, Fruit)
  deriving (Show, Eq)

-- | Field subtype of Snake.
--   Contains information about
--   SnakeSegment and its Direction  
data Snake
  = Snake (SnakeSegment, Direction)
  | NoSnake
  deriving (Show, Eq)

-- | Field subtype of Fruit.
data Fruit
  = Fruit
  | NoFruit
  deriving (Show, Eq)
