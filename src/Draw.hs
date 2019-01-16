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
        turtlePosition = (^.position) . snd
        turtleElevation = (^.elevation) . snd
        sortedByPosition = sortBy (compare `on` turtlePosition) (Map.toList (board^.path))
        groupedByPosition = groupBy ((==) `on` turtlePosition) sortedByPosition
        groupedAndSorted = map (map (fmap _position) . sortBy (compare `on` turtleElevation)) groupedByPosition
    setCursorPosition (row, column)
    setAnnotation (background $ dull Green)
    mapM_ (\c -> setAnnotation (background $ dull c) >> putChar '>') (take (end-start) (cycle [Green, Blue]))
    setAnnotation (background $ dull Red)
    putChar '#'
    flip mapM_ groupedAndSorted $ \xs -> do
        setCursorPosition (row, column + (snd . head $ xs))
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
        drawCard row column' size c) [0..] cards

drawCard :: Int -> Int -> (Int, Int) -> Card -> TerminalT IO ()
drawCard row column size card = do
    case card of
        (Move direction steps color) -> drawCard' row column size steps (show direction) color
        (Last steps) -> drawCard' row column size steps "↑" White
        (Any direction steps) -> drawCard' row column size steps (show direction) White
        (Blank color) -> drawCard' row column size Zero "" color

drawCard' :: Int -> Int -> (Int, Int) -> Steps -> String -> BasicColor -> TerminalT IO ()
drawCard' row column size steps text color = do
    drawSquare row column size color 
    drawContent row column size steps text

cardMatrix :: (Int, Int) -> [String]
cardMatrix (width, height) = 
            ["┌" ++ replicate width '─' ++ "┐"] ++
            replicate height ("│" ++ replicate width ' ' ++ "│") ++
            ["└" ++ replicate width '─' ++ "┘"]

drawContent :: Int -> Int -> (Int, Int) -> Steps -> String -> TerminalT IO ()
drawContent row column (width, height) steps text = do   
    resetAnnotations
    setCursorPosition (row + height `div` 2 + 1, column + width `div` 2 + 1)
    mapM_ (const $ putString text >> moveCursorUp 1 >> moveCursorLeft 1) [1..fromEnum steps]  

drawSquare :: Int -> Int -> (Int, Int) -> BasicColor -> TerminalT IO ()
drawSquare row column size@(width, _) color = do
        setCursorPosition (row, column)
        let backgroundColor = background $ dull color
            drawColoredBlank = setAnnotation backgroundColor >> putChar ' ' >> resetAnnotation backgroundColor
            drawRow = \line -> do
                mapM_ (\c -> if c == ' ' then drawColoredBlank else putChar c) line
                moveCursorLeft (width + 2)
                moveCursorDown 1
        mapM_ drawRow (cardMatrix size)

drawPlayer :: Int -> Int -> Player -> TerminalT IO ()
drawPlayer row column player = do
    setCursorPosition (row, column)
    setAnnotation (background $ dull (player^.color))
    putString $ "Player " ++ (show $ player^.number)
    resetAnnotations

isInCard :: (Int, Int) -> (Int, Int) -> Bool
isInCard (cursorRow, cursorColumn) (row, column) = 
    cursorRow >= row
    && cursorRow <= row + (cardHeight + 1) 
    && cursorColumn >= column 
    && cursorColumn <= column + (cardWidth + 1)

