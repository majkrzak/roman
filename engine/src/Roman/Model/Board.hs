module Roman.Model.Board
  ( pattern SIZE
  , Board
  , empty
  , BoardView
  , view
  , unview
  ) where

import Roman.Model.Field
import Roman.Model.Position
import Data.Viewable

import Prelude (Int, Show, Eq, id, foldMap, map)

-- | Predefined Board size.
pattern SIZE :: Int
pattern SIZE = 32

-- | Type of game board.
newtype Board
  = Board [(Position, Field)]
  deriving (Show, Eq)

-- | Polymorphic View of game board.
type BoardView e = View Board (Position, Field) e

instance Viewable Board (Position, Field) where
  view board = View board id

instance VFoldable Board (Position, Field) where
  vfold (View (Board l) kernel) = foldMap kernel l

instance UnViewable Board (Position, Field) where
  unview (View (Board l) kernel) = Board (map kernel l)

-- | Initialize empty Board.
empty :: Board
empty = Board
  [ (Position (i, j), Field (NoSnake, NoFruit))
  | i <- [1 .. SIZE]
  , j <- [1 .. SIZE]
  ]
