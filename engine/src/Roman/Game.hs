module Roman.Game where

import Roman.Model.Board (Board, empty, view, unview)
import Roman.Model.Position (Position(..))
import Roman.Model.Field (Field(..), Snake(..), Fruit(..))
import Roman.Model.SnakeSegment (SnakeSegment(Head, Body, Tail))
import Roman.Model.Direction (Direction(..))
import Roman.Model.Neighbours (neighbours, predecessor)
import Prelude (Int, Maybe(Just), ($), (.), (<$>), (+), sum)


-- | Type of Game state.
data Game = Game
  { board :: Board
  , score :: Int
  , time  :: Int
  }

-- | Initialize empty Game.
init :: Game
init = Game
  { board = unview $ initializer <$> view empty
  , score = 0
  , time  = 0
  }
 where
  initializer :: (Position, Field) -> (Position, Field)
  initializer (pos@(Position (1, 3)), _) =
    (pos, Field (Snake (Head, South), NoFruit))
  initializer (pos@(Position (1, 2)), _) =
    (pos, Field (Snake (Body, East), Fruit))
  initializer (pos@(Position (1, 1)), _) =
    (pos, Field (Snake (Tail, East), NoFruit))
  initializer (pos@(Position (7, 3)), _) = (pos, Field (NoSnake, Fruit))
  initializer (pos@(Position (_, _)), _) = (pos, Field (NoSnake, NoFruit))

-- | Update Game by one step.
step :: Game -> Game
step Game {..} = Game
  { board = unview $ advanceTail . advanceHead <$> view board
  , score = score + sum (points <$> view board)
  , time  = time + 1
  }
 where
  advanceHead :: (Position, Field) -> (Position, Field)
  advanceHead (p, Field (Snake (Head, direction), fruit)) =
    (p, Field (Snake (Body, direction), fruit))
  advanceHead (p@(neighbours board -> predecessor -> (Just (Field (Snake (Head, direction), _)))), Field (NoSnake, fruit))
    = (p, Field (Snake (Head, direction), fruit))
  advanceHead x = x
  advanceTail :: (Position, Field) -> (Position, Field)
  advanceTail (p, Field (Snake (Tail, direction), Fruit)) =
    (p, Field (Snake (Tail, direction), NoFruit))
  advanceTail (p, Field (Snake (Tail, _), NoFruit)) =
    (p, Field (NoSnake, NoFruit))
  advanceTail (p@(neighbours board -> predecessor -> (Just (Field (Snake (Tail, _), NoFruit)))), Field (Snake (Body, direction), fruit))
    = (p, Field (Snake (Tail, direction), fruit))
  advanceTail x = x
  points :: (Position, Field) -> Int
  points (neighbours board -> predecessor -> (Just (Field (Snake (Head, _), _))), Field (NoSnake, Fruit))
    = 1
  points _ = 1
