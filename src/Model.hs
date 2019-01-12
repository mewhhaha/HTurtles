{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Control.Monad.Terminal
import qualified Data.Map as Map
import Data.List.Split
import Lib
import Data.List
    
data Card = Move Int BasicColor | Any Int | Last Int
    deriving (Eq, Show)
type Deck = [Card]
data Turtle = Turtle {
    _sorting :: Int,
    _position :: Int
    } deriving Show
type Path = Map.Map BasicColor Turtle
data Player = Player {
    _number :: Int,
    _color :: BasicColor,
    _cards :: [Card] 
    } deriving (Eq, Show)
type Turns = [Player]
data Board = Board {
    _size :: (Int, Int),
    _path :: Path,
    _deck :: Deck,
    _discarded :: Deck
    } deriving Show

data World = World {
    _players :: Int,
    _board :: Board,
    _turns :: Turns
    } deriving Show

makeLenses ''World
makeLenses ''Board
makeLenses ''Player
makeLenses ''Turtle

currentPlayer :: World -> Player
currentPlayer = head . (^.turns)

turtleColors :: [BasicColor]
turtleColors = [Black, Red, Green, Yellow, Blue, Magenta, Cyan]         

defaultDeck :: Deck
defaultDeck = concatMap colorCards turtleColors
              ++ anyCards 
              ++ lastCards
    where colorCards c = [Move 2 c] ++ replicate 5 (Move 1 c) ++ replicate 2 (Move (-1) c)
          anyCards = replicate 5 (Any 1) ++ replicate 2 (Any (-1))
          lastCards = replicate 3 (Last 1) ++ replicate 2 (Last 2)

newWorld :: (Int, Int) -> Deck -> [BasicColor] -> Int -> World
newWorld newSize newDeck newTurtles nPlayers = World nPlayers (Board newSize newPath newDeck []) (cycle players)
    where startPosition = uncurry Turtle (dup . fst $ newSize)
          startHands = chunksOf 5 newDeck
          newPath = foldr (flip Map.insert startPosition) Map.empty newTurtles
          players = zipWith newPlayer [0..] $ take nPlayers newTurtles
          newPlayer i c = Player i c (startHands !! i)