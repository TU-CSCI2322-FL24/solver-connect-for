data Player = Red | Yellow deriving (Eq, Ord, Show)

type Cell = Maybe Player  

type Row = [Cell]

type Board = [Row]

data GameState = Board | Player | Maybe Move deriving (Show, Eq)

type Move = Int

data Outcome = Ongoing | Draw | Winner Player deriving (Eq, Show)

type Winner = Player

-- Takes the gamestate and the most recent move and checks if that player has won the game.
checkWin :: GameState -> Move -> Outcome
checkWin = undefined

-- Takes the gamestate, Player and the move and adds it to the board.
makeMove:: GameState -> Move -> Board
makeMove = undefined 

-- Tell you all the moves that can be made from the current gamestate, [0..6].
legalMoves :: GameState -> [Move]
legalMoves = undefined

-- Print the game state, should be in 2d list form. can represent players with X's and O's.
printGame :: GameState -> String 
printGame = undefined 
