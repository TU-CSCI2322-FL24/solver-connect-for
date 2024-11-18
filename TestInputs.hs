import Connect
import Solver


-- Can be used to test checkWin and makeMove.
board = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]


yHori = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Just Yellow, Just Yellow, Just Yellow, Just Yellow, Nothing, Nothing]]

rHori = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Just Red, Just Red, Just Red, Just Red, Nothing, Nothing]]


vertY = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing]]


posDiagY = [[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Nothing, Just Yellow, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Just Yellow, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing]]

antiDiagY = [[Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Just Yellow, Nothing, Nothing, Nothing, Nothing], 
        [Nothing, Nothing, Nothing, Just Yellow, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]]

drawBoard = [Just Yellow, Just Red, Just Red, Just Yellow, Just Red, Just Yellow, Just Red] : tail board


-- Test boards for whoWillWin, bestMove can be tested on these boards aswell.
whoWWY = 
        [
                [ Just Red,    Just Yellow,  Nothing,        Nothing,     Just Red,     Just Yellow,  Just Red],
                [ Nothing,     Nothing,      Just Yellow,    Nothing,     Nothing,      Nothing,      Nothing],
                [ Nothing,     Just Yellow,  Just Red,       Just Red,    Nothing,      Nothing,      Nothing],
                [ Just Yellow, Nothing,      Nothing,        Nothing,     Nothing,      Nothing,      Nothing],                
                [ Nothing,     Nothing,      Nothing,        Nothing,     Nothing,      Nothing,      Nothing],
                [ Nothing,     Nothing,      Nothing,        Nothing,     Nothing,      Nothing,      Nothing]
        ]

whoWWDraw = 
        [
                [ Just Red,    Just Yellow,  Nothing,    Nothing,     Just Red,     Just Yellow,  Just Red],
                [ Nothing,     Nothing,      Nothing,    Nothing,     Nothing,      Nothing,      Nothing],
                [ Nothing,     Just Yellow,  Just Red,   Just Red,    Nothing,      Nothing,      Nothing],
                [ Just Yellow, Nothing,      Nothing,    Nothing,     Nothing,      Nothing,      Nothing],                
                [ Nothing,     Nothing,      Nothing,    Nothing,     Nothing,      Nothing,      Nothing],
                [ Nothing,     Nothing,      Nothing,    Nothing,     Nothing,      Nothing,      Nothing]
        ]

whoWWSimple = 
        [
                [ Just Red,    Just Yellow,  Just Red,     Just Yellow,  Just Red,     Just Yellow,  Just Red],
                [ Nothing,     Nothing,      Just Yellow,  Just Red,     Nothing,      Nothing,      Nothing],
                [ Nothing,     Just Yellow,  Just Red,     Just Red,     Nothing,      Nothing,      Nothing],
                [ Just Yellow, Nothing,      Nothing,      Nothing,      Nothing,      Nothing,      Nothing],                
                [ Nothing,     Nothing,      Nothing,      Nothing,      Nothing,      Nothing,      Nothing],
                [ Nothing,     Nothing,      Nothing,      Nothing,      Nothing,      Nothing,      Nothing]
        ]

whoWWYHori = 
        [
                [ Just Red,    Just Red,    Just Red,    Nothing,         Nothing,      Just Yellow,  Just Yellow],
                [ Nothing,     Nothing,     Nothing,     Nothing,         Nothing,      Nothing,      Nothing],
                [ Nothing,     Nothing,     Nothing,     Just Yellow,     Just Red,     Nothing,      Nothing],
                [ Nothing,     Nothing,     Nothing,     Nothing,         Nothing,      Nothing,      Nothing],
                [ Nothing,     Nothing,     Nothing,     Nothing,         Nothing,      Nothing,      Nothing],
                [ Nothing,     Nothing,     Nothing,     Nothing,         Nothing,      Nothing,      Nothing]
        ]