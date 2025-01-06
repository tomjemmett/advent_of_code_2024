{-# LANGUAGE TemplateHaskell #-}

module Day16 (day16, day16TestInput) where

import Common
import Control.Lens hiding ((:<), (<&>))
import Control.Monad.RWS
import Data.HashMap.Strict qualified as M
import Data.HashSet qualified as S
import Data.List (find)
import Data.Maybe (fromJust, maybe)
import Data.PQueue.Prio.Min (MinPQueue ((:<)))
import Data.PQueue.Prio.Min qualified as PQ

type Grid = M.HashMap Point2d Bool

type SearchWriter = S.HashSet Point2d

type Visited = S.HashSet (Point2d, Direction)

data SearchState = SearchState
  { _visited :: Visited,
    _best :: Maybe Int
  }
  deriving (Show)

makeLenses ''SearchState

data Maze = Maze
  { _grid :: Grid,
    _start :: Point2d,
    _end :: Point2d
  }
  deriving (Show)

makeLenses ''Maze

mkMaze :: Grid -> Point2d -> Point2d -> Maze
mkMaze g s e =
  Maze
    { _grid = g,
      _start = s,
      _end = e
    }

data Reindeer = Reindeer
  { _position :: Point2d,
    _direction :: Direction,
    _cost :: Int,
    _path :: [Point2d]
  }
  deriving (Show)

makeLenses ''Reindeer

mkReindeer :: Point2d -> Direction -> Int -> [Point2d] -> Reindeer
mkReindeer p d c ps =
  Reindeer
    { _position = p,
      _direction = d,
      _cost = c,
      _path = ps
    }

type Search = RWS Maze SearchWriter SearchState

day16TestInput :: String
day16TestInput =
  "#################\n\
  \#...#...#...#..E#\n\
  \#.#.#.#.#.#.#.#.#\n\
  \#.#.#.#...#...#.#\n\
  \#.#.#.#.###.#.#.#\n\
  \#...#.#.#.....#.#\n\
  \#.#.#.#.#.#####.#\n\
  \#.#...#.#.#.....#\n\
  \#.#.#####.#.###.#\n\
  \#.#.#.......#...#\n\
  \#.#.###.#####.###\n\
  \#.#.#...#.....#.#\n\
  \#.#.#.#####.###.#\n\
  \#.#.#.........#.#\n\
  \#.#.#.#########.#\n\
  \#S#.............#\n\
  \#################\n\
  \"

day16 :: AOCSolution
day16 input = show <$> [p1, p2]
  where
    (p1, succ . S.size -> p2) = findAllShortestPaths $ parseInput input

-- take the input and convert it to our maze type
parseInput :: String -> Maze
parseInput input = mkMaze (M.map (/= '#') $ M.fromList i) s e
  where
    i = do
      (y, line) <- zip [0 ..] $ lines input
      (x, v) <- zip [0 ..] line
      pure ((y, x), v)
    Just s = fst <$> find ((== 'S') . snd) i
    Just e = fst <$> find ((== 'E') . snd) i

-- perform a search to find all of the shortest paths
findAllShortestPaths :: Maze -> (Int, SearchWriter)
findAllShortestPaths m = evalRWS (go initQ) m initS
  where
    -- get the start position
    s = m ^. start
    -- make the initial reindeer
    r = mkReindeer s East 0 [s]
    -- create the initial queue
    initQ = PQ.singleton 0 r
    -- create the initial state
    initS = SearchState {_visited = S.empty, _best = Nothing}
    -- perform the search
    go :: PQ.MinPQueue Int Reindeer -> Search Int
    -- when our queue is empty, get the size of the path from the state and return it
    go PQ.Empty = gets (fromJust . view best)
    -- extract the reindeer with minimum cost from the queue
    go ((_, r) :< q) = do
      -- what is the best path length (starts as Nothing, becomes Just x)
      b <- gets $ view best
      -- what is the end point we are looking for
      e <- asks $ view end
      -- get the next points this reindeer could move to
      ns <- PQ.fromList <$> nexts r
      -- update the queue
      let q' =
            -- if this reindeer has a cost greater than the best, exit the search. We will have
            -- found all the best paths
            if maybe True (r ^. cost <=) b
              then PQ.union ns q
              else PQ.empty
      -- insert into visited State the current reindeers position and direction
      modify $ over visited (S.insert (r ^. position, r ^. direction))
      -- if we have reached the end position
      when (r ^. position == e) do
        -- update the best value to be the cost of this reindeer
        modify $ over best (const $ Just $ r ^. cost)
        -- output the set of points in the path
        tell $ S.fromList $ r ^. path
      -- recursively call go with the modified queue
      go q'
    -- get the next reindeer positions to add to the priority queue
    -- returns a tuple containing the cost (the priority) and the reindeer
    nexts :: Reindeer -> Search [(Int, Reindeer)]
    nexts r = do
      g <- asks $ view grid
      v <- gets $ view visited
      pure $
        -- convert the next reindeer to be a tuple of the cost and the reindeer
        map (\n -> (n ^. cost, n)) $
          -- filter out invalid reindeer
          filter (isValid g v) $
            -- try the three possible new reindeer
            [moveReindeer, turnReindeerCW, turnReindeerCCW] <*> pure r
      where
        -- move the reindeer in the current direction
        moveReindeer :: Reindeer -> Reindeer
        moveReindeer r =
          let p = r ^. position
              d = r ^. direction
           in over path (p :) $ over cost (+ 1) $ over position (`moveOneStepInDir` d) r
        -- turn the reindeer clockwise, then move one step in that direction
        turnReindeerCW :: Reindeer -> Reindeer
        turnReindeerCW = moveReindeer . over cost (+ 1000) . over direction turn90
        -- turn the reindeer counter clockwise, then move one step in that direction
        turnReindeerCCW :: Reindeer -> Reindeer
        turnReindeerCCW = moveReindeer . over cost (+ 1000) . over direction turn270
        -- check whether this is a valid position for the reindeer; checking:
        --   - the space we are moving to is not a wall
        --   - the space we are moving to has not already been visited
        isValid :: Grid -> Visited -> Reindeer -> Bool
        isValid g v r =
          let p = r ^. position
              d = r ^. direction
           in (not ((p, d) `S.member` v) && g M.! p)