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


-- create deck

reshuffle :: World -> TerminalT IO World
reshuffle world = do
        shuffled <- lift $ shuffle (world^.board.discarded)
        let emptyDiscarded = world & board.discarded .~ []
        return $ emptyDiscarded & board.deck .~ shuffled

nextTurn :: Player -> World -> World
nextTurn player world = over turns (cycle . take (world^.players). (++ [player]) . tail) world

topDeck :: World -> TerminalT IO (Card, World)
topDeck world = case listToMaybe card of
                Just card -> return (card, world & board . deck .~ deck')
                Nothing -> do 
                    world' <- reshuffle world
                    topDeck world'
    where (card, deck') = splitAt 1 (world^.board.deck)    

chooseCard :: Int -> Player -> (Card, Player)
chooseCard i player = (card, over cards (filter (/= card)) player)
    where card = (player^.cards) !! i

discardCard :: Card -> World -> World
discardCard c w = over (board . discarded) (c:) w

addCard :: Card -> Player -> Player
addCard c p = over cards (c:) p

moveTurtle :: Int -> BasicColor -> World -> World
moveTurtle steps color w = if destination == turtle^.position then w else w & board . path .~ updatedPath
    where   currentPath = w ^. board . path
            (Just turtle) = Map.lookup color currentPath
            destination = clamp (w^.board.size) (turtle^.position + steps)
            carriedBy = flip all [(==) `on` (^.position), (<) `on` (^.sorting)] . (turtle &:)
            atDestination = (\c t -> color /= c && destination == t^.position)
            heightAtDestination = Map.size $ Map.filterWithKey atDestination currentPath
            turtlesCarried = Map.filter carriedBy currentPath 
            movedTurtles = 
                Map.insert color (Turtle heightAtDestination destination) 
                $ Map.map (\p -> Turtle (p^.sorting + heightAtDestination - turtle^.sorting) destination) turtlesCarried
            updatedPath = Map.union movedTurtles currentPath

playCard :: Card -> World -> TerminalT IO World
playCard (Move steps color) w = return $ moveTurtle steps color w
playCard (Last _) w = return w
playCard (Any _) w = return w 

checkWinner :: World -> Maybe Player
checkWinner world = do
    winner <- maybeWinner
    listToMaybe $ filter (\p -> p^.color == winner) allPlayers
 where end = snd $ world^.board.size
       allPlayers = take (world^.players) (world^.turns)
       turtleSorting = _sorting . snd
       turtlesOnEnd = Map.toList $ Map.filter ((==) end . (^.position)) (world^.board.path)
       maybeWinner = listToMaybe $ map fst $ sortBy (compare `on` turtleSorting) turtlesOnEnd

loop :: World -> TerminalT IO ()
loop world = do
    draw world
    event <- waitEvent
    updatedWorld <-
            case event of
                MouseEvent e -> case e of 
                    MouseButtonClicked cursor _ -> do
                           case isInCard cursor of
                             Nothing -> return world
                             Just i -> do
                                let (chosenCard, player') = chooseCard i (currentPlayer world)
                                (topCard, world') <- playCard chosenCard world >>= topDeck
                                return $ nextTurn (addCard topCard player') $ discardCard chosenCard world'
                    _ -> return world
                _ -> return world
    if quit event then resetAnnotations else loop updatedWorld

quit :: Event -> Bool
quit InterruptEvent = True
quit _ = False

main :: IO ()
main = do
    startDeck <- shuffle defaultDeck
    startColors <- shuffle turtleColors
    let world = newWorld (0, 10) startDeck startColors 3
    withTerminal $ \terminal -> 
        runTerminalT (loop world) terminal


        


