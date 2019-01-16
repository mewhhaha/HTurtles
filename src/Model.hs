{-# LANGUAGE TemplateHaskell #-}

module Model where

import Control.Lens
import Control.Monad.Terminal hiding (Direction)
import qualified Data.Map as Map
import Data.List.Split
import Lib
import Data.List

data Steps = Zero | One | Two
    deriving (Eq, Show, Enum)

data Direction = Forward | Backward
    deriving (Eq)

instance Enum Direction where
    fromEnum Forward = 1
    fromEnum Backward = -1
    toEnum 1 = Forward
    toEnum (-1) = Backward

instance Show Direction where
    show Forward = "+"
    show Backward = "-"

data Card = Move Direction Steps BasicColor | Any Direction Steps | Last Steps | Blank BasicColor
    deriving (Eq, Show)
type Deck = [Card]
data Turtle = Turtle {
    _elevation :: Int, -- Confusing name, please change to something else
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

defaultTurtleColors :: [BasicColor]
defaultTurtleColors = [Red, Green, Blue, Magenta, Cyan]         

defaultDeck :: Deck
defaultDeck = concatMap colorCards defaultTurtleColors
              ++ anyCards 
              ++ lastCards
    where colorCards c = [Move Forward Two c] ++ replicate 5 (Move Forward One c) ++ replicate 2 (Move Backward One c)
          anyCards = replicate 5 (Any Forward One) ++ replicate 2 (Any Backward One)
          lastCards = replicate 3 (Last One) ++ replicate 2 (Last Two)

defaultBoardSize = (0, 10)
defaultHandSize = 5

newDefaultWorld :: Int -> IO World
newDefaultWorld nPlayers = do
        shuffledDeck <- shuffle defaultDeck
        playerColors <- take nPlayers <$> shuffle defaultTurtleColors
        
        let (startCards, newDeck) = splitAt (nPlayers * 5) shuffledDeck
            startPosition = uncurry Turtle (dup . fst $ defaultBoardSize)
            
            newPlayers = zipWith3 Player [0..nPlayers] playerColors (chunksOf defaultHandSize startCards)
            newPath = foldr (flip Map.insert startPosition) Map.empty defaultTurtleColors

        return $ World nPlayers (Board defaultBoardSize newPath newDeck []) (cycle newPlayers)