module Connect where 

import Data.Maybe
import Data.List

data Player = Red | Yellow deriving (Eq, Ord, Show)

type Cell = Maybe Player  

type Row = [Cell]

type Board = [Row]

type GameState = (Board, Player)

type Move = Int

data Outcome = Ongoing | Draw | Winner Player deriving (Eq, Show)


-- Given a GameState, it will use the previous move to find the row in which it landed.
findRow :: GameState -> Int -> Int
findRow (xs:xss, player) col = aux (xs:xss) col 1
  where
    boardWidth = length xs 
    boardLength = length (xs:xss)
    aux [] _ _ = -1  
    aux (xs:xss) col rowIndex =
      case drop col xs of
        (Just p : _) | p == player -> boardLength - rowIndex  
        _                          -> aux xss col (rowIndex + 1)



-- Takes the gamestate and the most recent move and checks if that player has won the game.
checkWin :: GameState -> Outcome
checkWin (board, player) 
    | any (\f -> f (board, player)) [checkHorizontal, checkVertical, checkDiagonal] = Winner player
    | checkDraw board = Draw
    | otherwise = Ongoing

checkRowForWin :: Player -> Row -> Bool
checkRowForWin player (x:xs)
    | length (x:xs) < 4 = False  
    | take 4 (x:xs) == replicate 4 (Just player) = True  
    | otherwise = checkRowForWin player xs 

-- grabs the row, and checks if there are 4 consecutive player.
checkHorizontal :: GameState -> Bool
checkHorizontal ([],_) = False 
checkHorizontal (xs:xss, player) 
    | checkRowForWin player xs = True
    | otherwise = checkHorizontal (xss, player) 

-- Function to check if there are 4 consecutive pieces in the row for the player
-- grabs the column and checks if there are 4 consecutive player.
checkVertical :: GameState -> Bool
checkVertical (board, player) = any (checkRowForWin player) rotatedBoard
  where
    rotatedBoard = transpose board

checkDiagonal :: GameState -> Bool 
checkDiagonal (board, player) = any (checkRowForWin player) (convertDiagsToRows board) || 
                                any (checkRowForWin player) (convertDiagsToRows (map reverse board))

convertDiagsToRows :: Board -> [Row]
convertDiagsToRows board = [convertDiagToRow board col | col <- [0..(length (head board) - 1)]]

convertDiagToRow :: Board -> Int -> Row
convertDiagToRow [] _ = [] 
convertDiagToRow (xs : xss) col 
    | col >= length xs = []
    | otherwise = head (drop col xs) : convertDiagToRow xss (col + 1)

 -- To check if there is a draw simply check if the head of the board is full.
checkDraw :: Board -> Bool
checkDraw (xs:xss) = all isJust xs

-- Takes the gamestate, Player and the move and adds it to the board.
makeMove:: GameState -> Move -> GameState
makeMove (board, player) move = 
  let rotatedBoard = transpose board 
      (before, (column:after)) = splitAt move rotatedBoard
      newColumn = reverse $ updateColumn (reverse column) player
  in (transpose (before ++ (newColumn : after)), player)

updateColumn :: Row -> Player -> Row
updateColumn [] _ = [] 
updateColumn (Nothing:ys) player = Just player : ys    
updateColumn (y:ys) player = y : updateColumn ys player       
        



-- Tell you all the moves that can be made from the current gamestate, [0..6].
legalMoves :: GameState -> [Move]
legalMoves = undefined

-- Print the game state, should be in 2d list form. can represent players with X's and O's.
printGame :: GameState -> String 
printGame = undefined 
