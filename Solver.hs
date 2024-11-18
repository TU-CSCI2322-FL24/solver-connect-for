module Solver where
import Connect
import Data.Maybe

whoWillWin :: GameState -> Winner 
whoWillWin (board, player) = 
    case checkWin (board, switchPlayer player) of 
        Just result -> result 
        Nothing -> 
            let 
                moves = legalMoves board 
                outcomes = map (makeMove (board, player)) moves 
                results = map evaluateOutcomes outcomes 
            in findWinner player results 
    where 
        evaluateOutcomes (resBoard, _) = whoWillWin (resBoard, switchPlayer player) 
 



findWinner :: Player -> [Winner] -> Winner 
findWinner player results 
    | Winner player `elem` results = Winner player
    | Draw `elem` results = Draw 
    | otherwise = Winner $ switchPlayer player

bestMove :: GameState -> Move
bestMove (board, player) = undefined
       


