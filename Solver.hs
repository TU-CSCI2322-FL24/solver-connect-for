module Solver where
import Connect
import Data.Maybe
import Data.List

type Rating = Int


--------------------------------- bestMove ------------------------------------------------

whoWillWin :: GameState -> Winner 
whoWillWin (board, player) = 
    case checkWin (board, player) of 
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


--------------------------------- rateGame ------------------------------------------------

rateGame :: GameState -> Rating 
rateGame (board, player) 
    | intialScore == Just (Winner Yellow) = 20000
    | intialScore == Just (Winner Red) = -20000
    | intialScore == Just Draw = 40000
    | otherwise = scoreDirections board (Just Yellow) - scoreDirections board (Just Red) 
    where intialScore = checkWin (board, player)

-- get the total score for all possible directions
scoreDirections :: Board -> Maybe Player -> Int 
scoreDirections board player = 
    scoreDirection board player + 
    scoreDirection (transpose board) player + 
    scoreDirection (convertDiagsToRows board) player + 
    scoreDirection (convertDiagsToRows (map reverse board)) player

scoreDirection :: Board -> Maybe Player -> Int 
scoreDirection board player = 
    let opponent = if player == Just Red then Just Yellow else Just Red
        listOf4 = concatMap getSublists board
        withoutOpponent = filter (notElem opponent) listOf4 
    in playerCount withoutOpponent

playerCount :: [Row] -> Int 
playerCount board = 
    let 
        onlyPlayer = map (filter isJust) board
        scoreEachList = map length onlyPlayer 
        totals = map score scoreEachList
    in sum totals

score :: Int -> Int 
score n
    | n == 0 = 0 
    | n == 1 = 0 
    | n == 2 = 2 
    | otherwise = 3

getSublists ::  Row -> [Row]
getSublists (x:xs) 
    | length (x:xs) < 4 = []
    | otherwise = take 4 (x:xs) : getSublists xs  


--------------------------------- minimax ------------------------------------------------

whoMightWin :: GameState -> Int -> (Rating, Move)
whoMightWin (board, player) depth 
    | isEndState currentRating = (currentRating, -1)
    | otherwise =  getBestScore player scoresWithMoves
    where 
        currentRating = rateGame (board, player)
        moves = legalMoves board 
        outcomes = map (makeMove (board, player)) moves 
        scores = map (minimax (depth - 1) (player /= Yellow)) outcomes
        scoresWithMoves = zip scores moves

minimax :: Int -> Bool ->  GameState -> Rating 
minimax depth maximizing (board, prevPlayer) 
    | depth == 0 || isEndState currentRating = currentRating
    | maximizing = maximize (-21000) depth outcomes
    | otherwise = minimize 21000 depth outcomes
    where
        currentPlayer = switchPlayer prevPlayer
        currentRating = rateGame (board, prevPlayer)
        moves = legalMoves board 
        outcomes = map (makeMove (board, currentPlayer)) moves 

maximize :: Rating -> Int -> [GameState] -> Rating 
maximize best _ [] = best
maximize best depth (x:xs)
    | best == 20000 = 20000
    | otherwise = maximize (max best (minimax (depth - 1) False x)) depth xs

minimize:: Rating -> Int -> [GameState] -> Rating 
minimize best _ [] = best
minimize best depth (x:xs)
    | best == -20000 = -20000
    | otherwise = minimize (min best (minimax (depth - 1) True x)) depth xs

getBestScore :: Player -> [(Rating, Move)] -> (Rating, Move)
getBestScore player scores 
    | player == Red = minimumBy (\(score1, _) (score2,_) -> compare score1 score2) scores
    | otherwise     = maximumBy (\(score1, _) (score2, _) -> compare score1 score2) scores

isEndState :: Rating -> Bool
isEndState rate = rate `elem` [40000, 20000, -20000] 


--------------------------------- parsing ------------------------------------------------

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
       


