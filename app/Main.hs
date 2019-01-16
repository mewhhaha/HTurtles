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

data Action = PlayCard Int | DefineCard Int | NoAction
    deriving Show

data Scene a = forall b. Scene {
    draw :: World -> TerminalT IO (),
    input :: Event -> World -> a,
    change ::  a -> World -> Maybe (Scene b),
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
checkWinner world = do
    winner <- maybeWinner
    listToMaybe $ filter (\p -> p^.color == winner) allPlayers
 where end = snd $ world^.board.size
       allPlayers = take (world^.players) (world^.turns)
       turtleElevation = (^.elevation) . snd
       turtlesOnEnd = Map.toList $ Map.filter ((==) end . (^.position)) (world^.board.path)
       maybeWinner = listToMaybe $ map fst $ sortBy (compare `on` turtleElevation) turtlesOnEnd

pickColorScene :: Int -> Scene Action
pickColorScene i = Scene {
    input = inputPickColorScene,
    draw = drawPickColorScene i,
    update = updatePickColorScene,
    change = changePickColorScene    
}

inputPickColorScene _ _ = NoAction
changePickColorScene _ _ = Nothing
updatePickColorScene _ = return . id

drawPickColorScene :: Int ->  World -> TerminalT IO ()
drawPickColorScene i world = do
    clearScreen
    drawSquare 0 10 (11, 5) Black
    drawBoard 5 11 (world^.board)
    drawSquare 7 0 (45, 5) Black
    setCursorPosition (7, 18) >> putString "PICK A COLOR"
    let (card, _) = chooseCard i (currentPlayer world)
        colors = case card of
            (Last _) -> turtlesInLastPlace world
            (Any _ _) -> defaultTurtleColors
            _         -> undefined
    drawCard 8 1 (cardWidth, cardHeight) $ card
    mapM_ ((>> putString ">") . setCursorPosition) (zip [8..12] (repeat 9))
    drawCards 8 11 (map Blank colors)

turtlesInLastPlace :: World -> [BasicColor]
turtlesInLastPlace world = Map.keys $ Map.filter ((== lastPosition) . _position) currentPath
    where currentPath = world^.board.path
          lastPosition = _position $ head $ sortBy (compare `on` _position) $ Map.elems currentPath

mainScene :: Scene Action
mainScene = Scene {
    input  = inputMainScene,
    draw   = drawMainScene,
    update = updateMainScene, 
    change = changeMainScene
}

changeMainScene :: Action -> World -> Maybe (Scene Action)
changeMainScene (DefineCard i) _ = Just (pickColorScene i)
changeMainScene _ _ = Nothing

inputMainScene :: Event -> World -> Action
inputMainScene event world = do
    case event of
        MouseEvent e -> case e of 
            MouseButtonClicked cursor _ -> do
                    let clickCard = listToMaybe 
                                    $ catMaybes 
                                    $ zipWith (\i rc -> if isInCard cursor rc then Just i else Nothing) 
                                    [0..4] (zip (repeat 8) [0,7..])
                    case clickCard of
                        Nothing -> NoAction
                        Just i -> case fst $ chooseCard i (currentPlayer world) of
                                    (Move _ _ _) -> PlayCard i
                                    _          -> DefineCard i
            _ -> NoAction
        _ -> NoAction                     

updateMainScene :: Action -> World -> IO World
updateMainScene action world = do
    case action of
        PlayCard i -> do
            let (chosenCard, player') = chooseCard i (currentPlayer world)
            (topCard, world') <- topDeck $ playCard chosenCard world
            return $ nextTurn (addCard topCard player') $ discardCard chosenCard world'
        _ -> return world

drawMainScene :: World -> TerminalT IO ()
drawMainScene world = do
    hideCursor
    clearScreen
    drawSquare 0 10 (11, 5) Black
    drawBoard 5 11 (world^.board)
    drawPlayer 7 0 (currentPlayer world)
    drawCards 8 0 (currentPlayer world ^. cards)

quit :: Event -> Bool
quit InterruptEvent = True
quit _ = False

loop :: Scene a -> World -> TerminalT IO ()
loop scene@(Scene draw input change update) world = do
    draw world
    flush
    event <- waitEvent
    let action = input event world
        changedScene = change action world
    world' <- lift $ update action world
    if quit event 
        then resetAnnotations 
        else case changedScene of
                    Nothing -> loop scene world'
                    Just scene' -> loop scene' world'

main :: IO ()
main = do
    let nPlayers = 3  
    world <- newDefaultWorld nPlayers
    withTerminal $ \terminal -> 
        runTerminalT (loop mainScene world) terminal


        


