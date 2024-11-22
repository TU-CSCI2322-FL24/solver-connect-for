module Solver where
import Connect
import Data.Maybe

whoWillWin :: GameState -> Winner 
whoWillWin (board, player) = 
    case checkWin (board, player) of -- base case, if someone has won or there is a draw, recursion should stop. 
        Just result -> result 
        Nothing -> 
            let 
                moves = legalMoves board 
                outcomes = map (makeMove (board, player)) moves 
                results = map evaluateOutcomes outcomes 
            in bestOutcome player results 
    where 
        evaluateOutcomes (resBoard, _) = whoWillWin (resBoard, switchPlayer player) 
 

bestOutcome :: Player -> [Winner] -> Winner 
bestOutcome player results 
    | Winner player `elem` results = Winner player
    | Draw `elem` results = Draw 
    | otherwise = Winner $ switchPlayer player


-- This will fail if given a full board. Should return Maybe Move to deal with this.
bestMove :: GameState -> Move
bestMove (board, player) = 
    let 
        moves = legalMoves board
        outcomes = map (makeMove (board, player)) moves `zip` moves 
        results = [(whoWillWin (resBoard, switchPlayer player), move) | ((resBoard, _), move) <- outcomes]
        winningMove = lookup (Winner player) results 
        drawMove = lookup Draw results

    in case winningMove of 
        Just move -> move 
        Nothing -> case drawMove of 
            Just move -> move 
            Nothing -> snd (head results)


readGame :: String -> GameState 
readGame input =
    let 
        (status:xs) = lines input 
        currentPlayer = if drop 6 status == "Red" then Red else Yellow 
        board = map parseRow xs 

    in (board, currentPlayer)

parseRow :: String -> Row 
parseRow row = map parseCell (words row)

parseCell :: String -> Cell 
parseCell "R" = Just Red 
parseCell "Y" = Just Yellow
parseCell _ = Nothing 


showGame :: GameState -> String 
showGame (board, player) = 
    let 
        playerTurn = "Turn: " ++ (if player == Red then "Red" else "Yellow")
        boardStr = unlines $ map showRow board
    in 
        playerTurn ++ "\n" ++ boardStr
       


