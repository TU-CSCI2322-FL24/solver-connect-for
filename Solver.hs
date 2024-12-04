module Solver where
import Connect
import Data.Maybe

type Rating = Integer

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

-- returns -1 when board is full because using maybes would make putBestMove more complex then it needs to be.
bestMove :: GameState -> Move
bestMove (board, player) 
    | checkDraw board = -1 
    | otherwise =
        let 
            moves = legalMoves board
            outcomes = map (makeMove (board, player)) moves `zip` moves 
            results = [(whoWillWin (resBoard, switchPlayer player), move) | ((resBoard, _), move) <- outcomes]
            winningMove = lookup (Winner player) results 
            drawMove = lookup Draw results
            bestMoveCanidate = if isJust winningMove then winningMove else drawMove
        in case bestMoveCanidate of 
            Just move -> move 
            Nothing -> snd (head results)

-- Should focus on the GameState, dont look into the future. 
rateGame :: GameState -> Rating 
rateGame (board, player) 
    | possibleWinner == Just (Winner Yellow) = 20000
    | possibleWinner == Just (Winner Red) = -20000
    | possibleWinner == Just Draw = 0
    | otherwise = 12 
    where possibleWinner = checkWin (board, player)

countNHori :: Board -> Int 
countNHori = undefined 

countNVerti :: Board -> Int
countNVerti = undefined 

countNDiag :: Board -> Int
countNDiag = undefined 

getScore :: GameState -> Int
getScore = undefined

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
        "Turn: " ++ (if player == Red then "Red" else "Yellow") ++ "\n" ++ printGame board
       


