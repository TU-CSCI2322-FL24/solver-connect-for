import System.Environment (getArgs)
import Connect 
import Solver
import TestInputs

main :: IO ()
main = do 
    args <- getArgs 
    let path = head args 
    game <- loadGame path 
    putBestMove game  

writeGame :: GameState -> FilePath -> IO ()
writeGame game path = writeFile path (showGame game)

loadGame :: FilePath -> IO GameState 
loadGame path = do 
    fileContents <- readFile path 
    return $ readGame fileContents   

putBestMove :: GameState -> IO ()
putBestMove (board, player) = do 
    let move = bestMove (board, player)
    let (boardWithMove, newPlayer) = makeMove (board, player) move
    let outcome = whoWillWin (boardWithMove, switchPlayer player)
    if move == -1 
        then putStrLn "No valid moves available."
    else 
        putStrLn $ "Best move: " ++ show move
    putStrLn $ "Outcome: " ++ show outcome