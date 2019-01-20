{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}


module Main where


import Lib
import Draw
import Control.Lens
import Model
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Data.Function
import System.Terminal
import Control.Monad.Terminal
import Control.Monad.Trans.Class
import Data.Char

data MainAction = PlayCard Int | DefineCard [BasicColor] Int | NoAction
    deriving Show

data PickColorAction = PickColor BasicColor
    deriving Show

data Scene = forall a. Scene {
    draw :: World -> TerminalT IO (),
    input :: Event -> World -> Maybe a,
    change ::  a -> World -> Maybe Scene,
    update :: a -> World -> IO World
}

reshuffle :: World -> IO World
reshuffle world = do
        shuffled <- shuffle (world^.board.discarded)
        let emptyDiscarded = world & board.discarded .~ []
        return $ emptyDiscarded & board.deck .~ shuffled

nextTurn :: Player -> World -> World
nextTurn player world = over turns (tail . cycle . take (world^.players) . (player:) . tail) world

topDeck :: World -> IO (Card, World)
topDeck world = case listToMaybe card of
                Just c -> return (c, world & board . deck .~ deck')
                Nothing -> do 
                    world' <- reshuffle world
                    topDeck world'
    where (card, deck') = splitAt 1 (world^.board.deck)    

chooseCard :: Int -> Player -> (Card, Player)
chooseCard i player = (card, over cards (undup (++) . fmap tail . splitAt i) player)
    where card = (player^.cards) !! i

discardCard :: Card -> World -> World
discardCard c w = over (board . discarded) (c:) w

addCard :: Card -> Player -> Player
addCard c = over cards (c:)

moveTurtle :: Int -> BasicColor -> World -> World
moveTurtle steps color w = if destination == turtle^.position then w else w & board . path .~ updatedPath
    where   currentPath = w ^. board . path
            (Just turtle) = Map.lookup color currentPath
            destination = clamp (w^.board.size) (turtle^.position + steps)
            carriedBy = flip all [(==) `on` (^.position), (<) `on` (^.elevation)] . (turtle &:)
            atDestination c t = color /= c && destination == t^.position
            heightAtDestination = Map.size $ Map.filterWithKey atDestination currentPath
            turtlesCarried = Map.filter carriedBy currentPath
            movedTurtles = 
                Map.insert color (Turtle heightAtDestination destination) 
                $ Map.map (\p -> Turtle (p^.elevation + heightAtDestination - turtle^.elevation) destination) turtlesCarried
            updatedPath = Map.union movedTurtles currentPath

playCard :: Card -> World -> World
playCard (Move direction steps color) w = moveTurtle (fromEnum steps * fromEnum direction) color w
playCard _ _ = undefined

checkWinner :: World -> Maybe Player
checkWinner world = listToMaybe $ mapMaybe matchingPlayer $ sortBy (compare `on` turtleElevation) turtlesOnEnd
 where end = snd $ world^.board.size
       allPlayers = take (world^.players) (world^.turns)
       turtleElevation = (^.elevation) . snd
       turtlesOnEnd = Map.toList $ Map.filter ((==) end . (^.position)) (world^.board.path)
       matchingPlayer t = listToMaybe $ mapMaybe (\p -> if p^.color == fst t then Just p else Nothing) allPlayers
       

turtlesInLastPlace :: World -> [BasicColor]
turtlesInLastPlace world = Map.keys $ Map.filter ((== lastPosition) . _position) currentPath
    where   currentPath = world^.board.path
            lastPosition = _position $ head $ sortBy (compare `on` _position) $ Map.elems currentPath

winnerScene :: Player -> Scene
winnerScene player = Scene {
    input = \_ _ -> Nothing,
    draw = \_ -> clearScreen >> drawPlayer 0 0 player,
    update = \_ -> return . id,
    change = \_ _ -> Nothing
}

pickColorScene :: [BasicColor] -> Int -> Scene
pickColorScene colors i = Scene {
    input = inputPickColorScene colors,
    draw = drawPickColorScene i,
    update = updatePickColorScene i,
    change = changePickColorScene    
}

inputPickColorScene :: [BasicColor] -> Event -> p -> Maybe PickColorAction
inputPickColorScene colors event world = 
    case event of
        KeyEvent e _ -> case e of 
            CharKey '1' -> color 0
            CharKey '2' -> color 1
            CharKey '3' -> color 2
            CharKey '4' -> color 3
            CharKey '5' -> color 4
            CharKey '6' -> color 5
            _ -> Nothing
        _ -> Nothing     
    where color i = PickColor <$> (listToMaybe $ snd $ splitAt i colors)

changePickColorScene :: p -> p1 -> Maybe Scene
changePickColorScene _ _ = Just mainScene

updatePickColorScene :: Int -> PickColorAction -> World -> IO World
updatePickColorScene i (PickColor color) world = do
    let (chosenCard, player) = chooseCard i (currentPlayer world)
        definedCard = case chosenCard of
                    (Last steps) -> Move Forward steps color
                    (Any direction steps) -> Move direction steps color
                    _ -> undefined
    (topCard, world') <- topDeck $ playCard definedCard world
    return $ nextTurn (addCard topCard player) $ discardCard definedCard world'

drawPickColorScene :: Int ->  World -> TerminalT IO ()
drawPickColorScene i world = do
    clearScreen
    drawSquare 0 0 (11, 6) Black
    drawBoard 6 1 (world^.board)
    drawSquare 8 0 (52, 5) Black
    setCursorPosition (8, 20) >> putString "PICK A COLOR"
    let (card, _) = chooseCard i (currentPlayer world)
        colors = case card of
            (Last _)  -> turtlesInLastPlace world
            (Any _ _) -> defaultTurtleColors
            _         -> undefined
    drawCard 9 1 (cardWidth, cardHeight) $ card
    mapM_ ((>> putString ">") . setCursorPosition) (zip [9..13] (repeat 9))
    drawCards 9 11 (map Blank colors)

mainScene :: Scene
mainScene = Scene {
    input  = inputMainScene,
    draw   = drawMainScene,
    update = updateMainScene, 
    change = changeMainScene
}

changeMainScene :: MainAction -> World -> Maybe Scene
changeMainScene (DefineCard colors i) _ = Just (pickColorScene colors i)
changeMainScene _ world = do
    winner <- checkWinner world
    Just (winnerScene winner)

inputMainScene :: Event -> World -> Maybe MainAction
inputMainScene event world = 
    case event of
        KeyEvent e _ -> case e of 
            CharKey '1' -> play 0
            CharKey '2' -> play 1
            CharKey '3' -> play 2
            CharKey '4' -> play 3
            CharKey '5' -> play 4
            _ -> Nothing
        _ -> Nothing     
    where play i = case fst $ chooseCard i (currentPlayer world) of
                            (Move _ _ _) -> Just $ PlayCard i
                            (Last _)     -> Just $ DefineCard (turtlesInLastPlace world) i
                            (Any _ _)    -> Just $ DefineCard (defaultTurtleColors) i

updateMainScene :: MainAction -> World -> IO World
updateMainScene action world = do
    case action of
        PlayCard i -> do
            let (chosenCard, player') = chooseCard i (currentPlayer world)
            (topCard, world') <- topDeck $ playCard chosenCard world
            return $ nextTurn (addCard topCard player') $ discardCard chosenCard world'
        _ -> return world

drawMainScene :: World -> TerminalT IO ()
drawMainScene world = do
    clearScreen
    drawSquare 0 0 (11, 6) Black
    drawBoard 6 1 (world^.board)
    drawPlayer 8 0 (currentPlayer world)
    drawCards 9 0 (currentPlayer world ^. cards)

quit :: Event -> Bool
quit InterruptEvent = True
quit _ = False

start :: Scene -> World -> TerminalT IO ()
start scene@(Scene draw _ _ _) world = do
    hideCursor
    clearScreen
    draw world
    loop scene world

loop :: Scene -> World -> TerminalT IO ()
loop scene@(Scene draw input change update) world = do
    event <- waitEvent
    if quit event 
        then resetAnnotations 
        else case input event world of
                Nothing -> loop scene world
                Just action -> do
                    world' <- lift $ update action world
                    clearScreen
                    case change action world' of
                            Nothing -> draw world' >> loop scene world'
                            Just scene'@(Scene draw' _ _ _) -> draw' world' >> loop scene' world'

main :: IO ()
main = do
    let nPlayers = 3  
    world <- newDefaultWorld nPlayers
    withTerminal $ \terminal -> 
        runTerminalT (start mainScene world) terminal