module Connect where 
import Debug.Trace
import Data.Maybe
import Data.List

data Player = Red | Yellow deriving (Eq, Ord, Show)
type Cell = Maybe Player  
type Row = [Cell]
type Board = [Row]
type GameState = (Board, Player)
type Move = Int
data Winner = Draw | Winner Player deriving (Eq, Show)


switchPlayer :: Player -> Player 
switchPlayer player 
  | player == Red = Yellow 
  | otherwise = Red 

legalMoves :: Board -> [Move]
legalMoves (xs:_) = [col | (col, cell) <- zip [0..6] xs, isNothing cell]

  
--------------------------------- checkWin ------------------------------------------------

checkWin :: GameState -> Maybe Winner
checkWin (board, _) 
    | any (\f -> f (board, Red)) [checkHorizontal, checkVertical, checkDiagonal] = Just (Winner Red)
    | any (\f -> f (board, Yellow)) [checkHorizontal, checkVertical, checkDiagonal] = Just (Winner Yellow)
    | checkDraw board = Just Draw
    | otherwise = Nothing
  
checkRowForWin :: Player -> Row -> Bool
checkRowForWin player (x:xs)
    | length (x:xs) < 4 = False  
    | take 4 (x:xs) == replicate 4 (Just player) = True  
    | otherwise = checkRowForWin player xs 

checkHorizontal :: GameState -> Bool
checkHorizontal ([],_) = False 
checkHorizontal (xs:xss, player) 
    | checkRowForWin player xs = True
    | otherwise = checkHorizontal (xss, player) 

checkVertical :: GameState -> Bool
checkVertical (board, player) = any (checkRowForWin player) rotatedBoard
  where
    rotatedBoard = transpose board

checkDiagonal :: GameState -> Bool 
checkDiagonal (board, player) = any (checkRowForWin player) (convertDiagsToRows board) || 
                                any (checkRowForWin player) (convertDiagsToRows (map reverse board))

convertDiagsToRows :: Board -> [Row]
convertDiagsToRows board = 
  let 
    converted = [convertDiagToRow (drop row board) col | row <- [0..length board - 1], col <- [0..(length (head board) - 1)]]
  in filter (\x -> length x == 4) converted 
  
convertDiagToRow :: Board -> Int -> Row
convertDiagToRow [] _ = [] 
convertDiagToRow (xs : xss) col 
    | col >= length xs = []
    | otherwise = head (drop col xs) : convertDiagToRow xss (col + 1)

checkDraw :: Board -> Bool
checkDraw (xs:xss) = all isJust xs


--------------------------------- makeMove ------------------------------------------------

makeMove:: GameState -> Move -> GameState
makeMove (board, player) move = 
  let rotatedBoard = transpose board 
      (before, (column:after)) = splitAt move rotatedBoard
      newColumn = tail $ updateColumn column player 
  in (transpose (before ++ (newColumn : after)), player)

updateColumn :: Row -> Player -> Row
updateColumn [] player = [Just player] 
updateColumn (Just x:ys) player = Just player : Just x : ys
updateColumn (Nothing:ys) player = Nothing : updateColumn ys player


--------------------------------- printGame -----------------------------------------------

showRow :: Row -> String
showRow row = unwords (map cellToString row)

cellToString :: Cell -> String
cellToString (Just Red) = "R"
cellToString (Just Yellow) = "Y"
cellToString Nothing = "0"

printGame :: Board -> String
printGame board = unlines (map showRow board)
