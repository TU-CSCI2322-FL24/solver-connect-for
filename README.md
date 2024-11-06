# solver-connect-for
solver-connect-for created by GitHub Classroom
 * Elliott Zorn
 * Joss Akinkunmi
 * Erik Cameron Rankin

 * data Peice = Empty | Player1 | Player2  deriving (Show, Ord, Eq)
 * data Player = Player1 | Player2 deriving (Show, Eq, Order)
 * data GameStatus = Going | Draw | Winner Player deriving (Show, Eq, Order)
 * data Game = Board | CurrentPlayer Player | Status GameStatus deriving (Show, Eq, Order)
 * type Column = [Piece]
 * type Board = [Column]
 * type Point = (Int, Int)
 * row ((1-7), row)
 * collumn (colNum, (number of tokens in the collum)) 
