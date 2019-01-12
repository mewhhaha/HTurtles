{-# LANGUAGE LambdaCase #-}


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

data Scene = Scene {
    _input :: Event -> World -> Action,
    _change :: Action -> World -> Scene,
    _update :: Action -> World -> IO World,
    _draw :: World -> TerminalT IO ()
}
-- create deck

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
            carriedBy = flip all [(==) `on` (^.position), (<) `on` (^.sorting)] . (turtle &:)
            atDestination c t = color /= c && destination == t^.position
            heightAtDestination = Map.size $ Map.filterWithKey atDestination currentPath
            turtlesCarried = Map.filter carriedBy currentPath 
            movedTurtles = 
                Map.insert color (Turtle heightAtDestination destination) 
                $ Map.map (\p -> Turtle (p^.sorting + heightAtDestination - turtle^.sorting) destination) turtlesCarried
            updatedPath = Map.union movedTurtles currentPath

playCard :: Card -> World -> World
playCard (Move steps color) w = moveTurtle steps color w
playCard _ _ = undefined

checkWinner :: World -> Maybe Player
checkWinner world = do
    winner <- maybeWinner
    listToMaybe $ filter (\p -> p^.color == winner) allPlayers
 where end = snd $ world^.board.size
       allPlayers = take (world^.players) (world^.turns)
       turtleSorting = _sorting . snd
       turtlesOnEnd = Map.toList $ Map.filter ((==) end . (^.position)) (world^.board.path)
       maybeWinner = listToMaybe $ map fst $ sortBy (compare `on` turtleSorting) turtlesOnEnd

loop :: Scene -> World -> TerminalT IO ()
loop scene world = do
    _draw scene world
    event <- waitEvent
    let action = _input scene event world
        scene' = _change scene action world
    world' <- lift $ _update scene action world
    if quit event then resetAnnotations else loop scene' world'


colorPickerScene :: Int -> Scene
colorPickerScene = undefined

mainScene :: Scene
mainScene = Scene {
    _input = inputMainScene,
    _draw = drawMainScene,
    _update = updateMainScene,
    _change = changeMainScene
}

changeMainScene :: Action -> World -> Scene
changeMainScene (DefineCard i) _ = colorPickerScene i 
changeMainScene _ _ = mainScene

inputMainScene :: Event -> World -> Action
inputMainScene event world = do
    case event of
        MouseEvent e -> case e of 
            MouseButtonClicked cursor _ -> do
                   case isInCard cursor of
                     Nothing -> NoAction
                     Just i -> case fst $ chooseCard i (currentPlayer world) of
                                    (Move _ _) -> PlayCard i
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

quit :: Event -> Bool
quit InterruptEvent = True
quit _ = False

main :: IO ()
main = do
    startDeck <- shuffle defaultDeck
    startColors <- shuffle turtleColors
    let world = newWorld (0, 10) startDeck startColors 3
    withTerminal $ \terminal -> 
        runTerminalT (loop mainScene world) terminal


        


