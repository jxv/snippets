{-# LANGUAGE RecordWildCards #-}

module Smooth (main) where

import Control.Applicative
import Control.Arrow

import Data.Function (fix)
import Data.Vector (Vector, (!), (!?), (//))
import Data.Set (Set)
import Data.Graph.AStar (aStar) -- astar package

import qualified Data.Vector        as V
import qualified Data.Vector      as Vector
import qualified Data.Set           as Set
import qualified Data.Maybe         as M


data Space = X | O deriving (Eq, Enum, Bounded, Show)


data Plan = Plan
    { cost      :: Int
    , grid      :: Vector (Vector Space)
    , initial   :: (Int,Int)
    , goal      :: (Int,Int)
    , heuristic :: Vector (Vector Int)
    } deriving (Show, Eq)


makePlan :: Vector (Vector Space) -> (Int,Int) -> (Int,Int) -> Int -> Plan
makePlan grid initial goal cost = Plan
    { cost = cost
    , grid = grid
    , initial = initial
    , goal = goal
    , heuristic = makeHeuristic grid goal cost
    }


makeHeuristic :: Vector (Vector Space) -> (Int,Int) -> Int -> Vector (Vector Int)
makeHeuristic grid (goalX, goalY) _ = V.fromList . map V.fromList $ let
    height = V.length grid
    width = V.length (grid ! 0)
    in [[ abs (goalX - i) + abs (goalY - j)
            | j <- init [0..width]] | i <- init [0..height]]


findPath :: Plan -> Vector (Int,Int)
findPath Plan{..} = let
    height = V.length grid
    width = V.length $ grid!0
    neighbors (x,y)
        = Set.fromList
        . filter (\(a,b) -> a >= 0 && a < height && b>=0 && b < width && grid!a!b /= X)
        $ [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
    distance (x,y) (v,w) = abs (x - v) + abs (v - y)
    lookupH (x,y) = heuristic!x!y
    in V.fromList $ (:) initial $
            M.fromMaybe [] $ aStar neighbors distance lookupH (== goal) initial


weightData, weightSmooth, tolerance :: Floating a => a
weightData = 0.1
weightSmooth = 0.2
tolerance = 0.000001


smoothPath :: Vector (Int,Int) -> Vector (Float,Float)
smoothPath path = let
    spath = V.map (fromIntegral *** fromIntegral) path :: Vector (Float,Float)
    indices = [1..((V.length path) - 2)]
    step sp f i = let
        idx i = f <$> (sp!?i)
        in smoothStep (f $ spath!i) (idx $ i - 2) (idx $ i - 1)
                                    (f (sp ! i))
                                    (idx $ i + 1) (idx $ i + 2)
    stepper i (sp, d) =
        let (x, dx) = step sp fst i
            (y, dy) = step sp snd i
        in (sp // [(i,(x,y))], d + dx + dy)
    in ($ spath) $ fix $ \loop sp -> let
            (sp', d) = foldr stepper (sp, 0) indices
            in if d >= tolerance then loop sp' else sp


weighData org cur = weightData * (org - cur)
weighSmooth cur prev next = weightSmooth * (prev + next - 2 * cur)
weighExtSmooth cur i i2 = 0.5 * weightSmooth * (2 * i - i2 - cur)


smoothStep :: Floating a => a -> Maybe a -> Maybe a -> a -> Maybe a -> Maybe a -> (a, a)
smoothStep org mprev2 mprev cur mnext mnext2
    = (\c -> (c, (abs $ cur - c)))
    . weighMay weighExtSmooth mnext mnext2
    . weighMay weighExtSmooth mprev mprev2
    . weighMay weighSmooth mprev mnext
    $ cur + weighData org cur
 where
    weighMay f i j c = c + (M.fromMaybe 0 $ (f c) <$> i <*> j)


myGrid :: Vector (Vector Space)
myGrid = V.fromList . map V.fromList $
    [[O,X,O,O,O,O]
    ,[O,X,O,X,X,O]
    ,[O,X,O,X,O,O]
    ,[O,O,O,X,O,X]
    ,[O,X,O,X,O,O]
    ]


myInit, myGoal :: (Int,Int)
myInit = (0,0)
myGoal = (4,5)


myPlan = makePlan myGrid myInit myGoal 1
myPath = findPath myPlan
mySPath = smoothPath myPath


printTransition :: IO ()
printTransition = mapM_ (\(p,sp) -> putStrLn $ show p ++ " -> " ++ show sp) $
    zip (V.toList myPath) (V.toList mySPath)


main :: IO ()
main = printTransition
