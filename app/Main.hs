{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (putChar)
import Lib
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Function
import System.Terminal
import Control.Monad.Terminal

type Turtle = BasicColor
data Card = Move Int Turtle | Any Int | Last Int
    deriving Show
type Deck = [Card]
type Path = Map.Map Turtle (Int, Int)
data Player = Player {
    number :: Int,
    color :: Turtle,
    cards :: [Card] 
} deriving Show
type Turns = [Player]
type Board = (Path, Deck)
type World = (Board, Turns)


-- Use lenses
-- create deck
start = 0
end = 10

nextTurn :: World -> World
nextTurn = fmap tail

currentPlayer :: World -> Player
currentPlayer = head . snd

getPath :: World -> Path
getPath = fst . fst 

replacePath :: Path -> World -> World
replacePath path ((_, deck), turns) = ((path, deck), turns)

drawCard :: World -> (Maybe Card, World)
drawCard ((path, deck), players) = (listToMaybe card, (board', players))
    where board' = (path, deck')
          (card, deck') = splitAt 1 deck

newWorld :: Deck -> [Player] -> World
newWorld deck players = ((path, deck), cycle players)
    where path = foldr (\player -> Map.insert (color player) (0, 0)) Map.empty players

moveTurtle :: Int -> Turtle -> World -> World
moveTurtle steps turtle w = replacePath path' w
    where   path = getPath w
            (Just (sorting, current)) = Map.lookup turtle path
            next = clamp start end (current + steps) -- Next position
            height = Map.size $ Map.filterWithKey (\t (_, p) -> t /= turtle && p == next) path -- The height of the tile, the number of turtles on the tile
            carried = Map.filter (\(s, p) -> p == current && s > sorting) path -- Turtles carried by the turtle
            moved = Map.insert turtle (height, next) $ Map.map (\(s, _) -> (s + height - sorting, next)) carried -- Turtles updated by movement
            path' = Map.union moved path

playCard :: Card -> World -> TerminalT IO World
playCard (Move steps turtle) w = return $ moveTurtle steps turtle w
playCard (Last _) w = return w
playCard (Any _) w = return w

drawPath :: Path -> TerminalT IO ()
drawPath path = do
    let position = snd . snd
        sorting = fst . snd
        sortedByPosition = sortBy (compare `on` position) (Map.toList path)
        groupedByPosition = groupBy ((==) `on` position) sortedByPosition
        groupedAndSorted = map (map (fmap snd) . sortBy (compare `on` sorting)) groupedByPosition
    setCursorPosition (5, 0)
    setAnnotation (background $ dull Green)
    mapM_ (const (putChar '#')) [start..end]
    flip mapM_ groupedAndSorted $ \xs -> do
        setCursorPosition (5, (snd . head $ xs))
        drawStack (map fst xs)
    setCursorPosition (6, 0)
    resetAnnotations
    putString $ show path

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
                    ArrowKey Rightwards -> playCard (Move 1 Red) world >>= playCard (Move 1 Blue)
                    ArrowKey Leftwards -> playCard (Move (-1) Red) world
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
    drawPath (getPath world)

main :: IO ()
main = do
    let world = newWorld [] [Player 0 Red [], Player 1 Blue []]
    withTerminal $ \terminal -> 
        runTerminalT (loop world) terminal
    



