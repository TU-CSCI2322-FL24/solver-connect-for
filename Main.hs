import System.Environment (getArgs)
import Connect 
import Solver

main :: IO ()
main = do 
    args <- getArgs 
    path <- if null args then do 
               putStrLn "Enter the filepath again:"
               getLine 
            else return $ head args 
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
    let
        move = bestMove (board, player) 
        (boardWithMove, newPlayer) = makeMove (board, player) move
        outcome = whoWillWin (boardWithMove, switchPlayer player) 
    putStrLn $ "Best move: " ++ show move 
    putStrLn $ "Outcome: " ++ show outcome  




