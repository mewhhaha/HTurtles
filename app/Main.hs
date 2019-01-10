{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (putChar)
import Lib
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Function
import System.Terminal
import Control.Monad.Terminal
import Control.Lens

type Turtle = BasicColor
data Card = Move Int Turtle | Any Int | Last Int
    deriving Show
type Deck = [Card]
data Placement = Placement {
    _sorting :: Int,
    _position :: Int
} deriving Show
type Path = Map.Map Turtle Placement
data Player = Player {
    _number :: Int,
    _color :: Turtle,
    _cards :: [Card] 
} deriving Show
type Turns = [Player]
data Board = Board {
    _size :: (Int, Int),
    _path :: Path,
    _deck :: Deck
} deriving Show

data World = World {
    _board :: Board,
    _turns :: Turns
} deriving Show

makeLenses ''World
makeLenses ''Board
makeLenses ''Player
makeLenses ''Placement

-- create deck

nextTurn :: World -> World
nextTurn = over turns tail

drawCard :: World -> (Maybe Card, World)
drawCard w = (listToMaybe card, w & board . deck .~ deck')
    where (card, deck') = splitAt 1 (w^.board.deck)

newWorld :: (Int, Int) -> Deck -> [Player] -> World
newWorld newSize newDeck players = World (Board newSize newPath newDeck) (cycle players)
    where newPath = foldr (\player -> Map.insert (player^.color) startPosition) Map.empty players
          startPosition = uncurry Placement (dup . fst $ newSize)

moveTurtle :: Int -> Turtle -> World -> World
moveTurtle steps turtle w = if destination == placement^.position then w else w & board . path .~ updatedPath
    where   currentPath = w ^. board . path
            (Just placement) = Map.lookup turtle currentPath
            destination = clamp (w^.board.size) (placement^.position + steps)
            turtlesAtDestination = Map.size $ Map.filterWithKey (\t p -> t /= turtle && p^.position == destination) currentPath
            turtlesCarried = Map.filter (\p -> p^.position == placement^.position && p^.sorting > placement^.sorting) currentPath 
            movedTurtles = 
                Map.insert turtle (Placement turtlesAtDestination destination) 
                $ Map.map (\p -> Placement (p^.sorting + turtlesAtDestination - placement^.sorting) destination) turtlesCarried
            updatedPath = Map.union movedTurtles currentPath

playCard :: Card -> World -> TerminalT IO World
playCard (Move steps turtle) w = return $ moveTurtle steps turtle w
playCard (Last _) w = return w
playCard (Any _) w = return w

drawBoard :: Board -> TerminalT IO ()
drawBoard board = do
    let (start, end) = board^.size
        sortedByPosition = sortBy (compare `on` (_position . snd)) (Map.toList (board^.path))
        groupedByPosition = groupBy ((==) `on` (_position . snd)) sortedByPosition
        groupedAndSorted = map (map (over _2 _position) . sortBy (compare `on` (_sorting . snd))) groupedByPosition
    setCursorPosition (5, 0)
    setAnnotation (background $ dull Green)
    mapM_ (const (putChar '#')) [start..end]
    flip mapM_ groupedAndSorted $ \xs -> do
        setCursorPosition (5, (snd . head $ xs))
        drawStack (map fst xs)
    setCursorPosition (6, 0)
    resetAnnotations
    putString $ show (board^.path)

drawStack :: [Turtle] -> TerminalT IO ()
drawStack turtles = 
    flip mapM_ turtles $ \t -> do
        drawTurtle t
        moveCursorLeft 1 
        moveCursorUp 1

drawTurtle :: Turtle -> TerminalT IO ()
drawTurtle turtle = do
    setAnnotation (background $ dull turtle) 
    putChar '@' 

loop :: World -> TerminalT IO ()
loop world = do
    draw world
    event <- waitEvent
    world' <-
            case event of
                KeyEvent e _ -> case e of 
                    CharKey 'r' -> playCard (Move 1 Red) world
                    CharKey 'b' -> playCard (Move 1 Blue) world
                    CharKey 'g' -> playCard (Move 1 Green) world
                    CharKey 'R' -> playCard (Move (-1) Red) world
                    CharKey 'B' -> playCard (Move (-1) Blue) world
                    CharKey 'G' -> playCard (Move (-1) Green) world
                    _ -> return world
                _ -> return world    
    if quit event then resetAnnotations else loop world'



quit :: Event -> Bool
quit = \case 
    InterruptEvent -> True
    _ -> False        

draw :: World -> TerminalT IO ()
draw world = do
    clearScreen
    drawBoard (world^.board)

main :: IO ()
main = do
    let world = newWorld (0, 10) [] [Player 0 Red [], Player 1 Blue [], Player 2 Green []]
    withTerminal $ \terminal -> 
        runTerminalT (loop world) terminal
    



