# solver-connect-for
solver-connect-for created by GitHub Classroom
 * Elliott Zorn
 * Joss Akinkunmi
 * Erik Cameron Rankin

 * data Piece = Empty | Player1 | Player2  deriving (Show, Ord, Eq)
 * data Player = Player1 | Player2 deriving (Show, Eq, Ord)
 * data GameStatus = Going | Draw | Winner Player deriving (Show, Eq, Ord)
 * data Game = Board | CurrentPlayer Player | Status GameStatus deriving (Show, Eq, Ord)
 * type Column = [Piece]
 * type Board = [Column]
 * type Point = (Int, Int)
 * -- Competitions --
 * type Cell = Maybe Player
 * data Player = Red | Yellow deriving (Show, Ord, Eq)
 * type Board = [[Cell]]
 * row ((1-7), row)
 * collumn (colNum, (number of tokens in the collum)) 
