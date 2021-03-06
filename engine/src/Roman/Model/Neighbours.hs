module Roman.Model.Neighbours
  ( Neighbours(..)
  , neighbours
  , predecessor
  ) where

import Roman.Model.Field (Field(Field), Snake(Snake))
import Roman.Model.Board (Board, pattern SIZE, view)
import Roman.Model.Position (Position(Position))
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Roman.Model.Direction (Direction(North, East, South, West))

-- | Neighbourhood of Board Field.
data Neighbours = Neighbours
  { northern :: Field
  , eastern  :: Field
  , southern :: Field
  , western  :: Field
  }
  deriving (Show, Eq)

-- | Constructs Neighbourhood for given Field.
neighbours :: Board -> Position -> Neighbours
neighbours b (Position (i, j)) = Neighbours
  { northern = fld $ Position (if i == 1 then SIZE else i - 1, j)
  , eastern  = fld $ Position (i, if j == SIZE then 1 else j + 1)
  , southern = fld $ Position (if i == SIZE then 1 else i + 1, j)
  , western  = fld $ Position (i, if j == 1 then SIZE else j - 1)
  }
 where
  fld :: Position -> Field
  fld p = snd $ fromJust $ find (\(p', _) -> p == p') $ view b

-- | Searches for predecessor field.
predecessor :: Neighbours -> Maybe Field
predecessor Neighbours { northern = f@(Field (Snake (_, South), _)) } = Just f
predecessor Neighbours { eastern = f@(Field (Snake (_, West), _)) } = Just f
predecessor Neighbours { southern = f@(Field (Snake (_, North), _)) } = Just f
predecessor Neighbours { western = f@(Field (Snake (_, East), _)) } = Just f
predecessor _ = Nothing
