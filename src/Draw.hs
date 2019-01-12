{-# LANGUAGE LambdaCase #-}

module Draw where    

import Prelude hiding (putChar)
import Control.Lens
import Control.Monad.Terminal
import qualified Data.Map as Map
import Model
import Data.List
import Data.Function

cardWidth = 5
cardHeight = 3

drawBoard :: Int -> Int -> Board -> TerminalT IO ()
drawBoard row column board = do
    let (start, end) = board^.size
        turtlePosition = _position . snd
        turtleSorting = _sorting . snd
        removeSortingNumber = map (fmap _position)
        sortedByPosition = sortBy (compare `on` turtlePosition) (Map.toList (board^.path))
        groupedByPosition = groupBy ((==) `on` turtlePosition) sortedByPosition
        groupedAndSorted = map (removeSortingNumber . sortBy (compare `on` turtleSorting)) groupedByPosition
    setCursorPosition (row, 0)
    setAnnotation (background $ dull Green)
    mapM_ (const (putChar '#')) [start..end]
    flip mapM_ groupedAndSorted $ \xs -> do
        setCursorPosition (row, (snd . head $ xs))
        drawStack (map fst xs)
    setCursorPosition (row + 1, column)
    resetAnnotations

drawStack :: [BasicColor] -> TerminalT IO ()
drawStack colors = 
    flip mapM_ colors $ \c -> do
        drawTurtle c
        moveCursorLeft 1 
        moveCursorUp 1

drawTurtle :: BasicColor -> TerminalT IO ()
drawTurtle color = do
    setAnnotation (background $ dull color) 
    putChar '@' 

drawCards :: Int -> Int -> [Card] -> TerminalT IO ()
drawCards row column cards = do
    let size = (cardWidth, cardHeight)
    sequence_ $ zipWith (\i c -> do
        let column' = column + (fst size + 2) * i
        setCursorPosition (row, column')
        drawCard size c) [0..] cards

drawCard :: (Int, Int) -> Card -> TerminalT IO ()
drawCard size = \case
    (Move steps color) -> drawCard' size (abs steps) (if steps > 0 then '+' else '-') color
    (Last steps) -> drawCard' size (abs steps) '↑' White
    (Any steps) -> drawCard' size (abs steps) (if steps > 0 then '+' else '-') White

cardMatrix :: (Int, Int) -> [String]
cardMatrix (width, height) = 
            ["┌" ++ replicate width '─' ++ "┐"] ++
            replicate height ("│" ++ replicate width ' ' ++ "│") ++
            ["└" ++ replicate width '─' ++ "┘"]

drawCard' :: (Int, Int) -> Int -> Char -> BasicColor -> TerminalT IO ()
drawCard' size@(width, height) steps text color = do
    let backgroundColor = background $ dull color
        drawColoredBlank = setAnnotation backgroundColor >> putChar ' ' >> resetAnnotation backgroundColor
        drawRow = \line -> do
            mapM_ (\c -> if c == ' ' then drawColoredBlank else putChar c) line
            moveCursorLeft (width + 2)
            moveCursorDown 1
    mapM_ drawRow (cardMatrix size)
    resetAnnotation (background $ dull color)
    moveCursorRight (width `div` 2 + 1)
    moveCursorUp (height `div` 2 + 1)
    mapM_ (const $ putChar text >> moveCursorUp 1 >> moveCursorLeft 1) [1..steps]

drawPlayer :: Int -> Int -> Player -> TerminalT IO ()
drawPlayer row column player = do
    setCursorPosition (row, column)
    setAnnotation (background $ dull (player^.color))
    putString $ "Player " ++ (show $ player^.number)
    resetAnnotations

isInCard :: (Int, Int) -> Maybe Int
isInCard (row, column) = if row > 7 && row < 13 && column < 36 then Just (column `div` 7) else Nothing

draw :: World -> TerminalT IO ()
draw world = do
    hideCursor
    clearScreen
    drawBoard 5 0 (world^.board)
    drawPlayer 7 0 (currentPlayer world)
    drawCards 8 0 (currentPlayer world ^. cards)
    setCursorPosition (14, 0)
    putString $ show $ head $ world^.turns
    --showCursor