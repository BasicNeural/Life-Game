import Graphics.UI.GLUT
import Data.List

import System.Exit
import Data.IORef

nearCell :: (Ord t, Floating t) => [(t, t)] -> (t, t) -> Int
nearCell world (x, y) = length $ filter isNear world
    where isNear (new_x, new_y) = dist < 2 && dist >= 1
              where dist = sqrt $ (x - new_x)**2 + (y - new_y)**2

execute world = filter (willAlive (\x -> x == 2 || x == 3)) world 
                        `union` filter (willAlive (==3) ) deadCell
    where deadCell = foldl1' union $ map (\(x, y) -> 
                    [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]) world
          willAlive f x = f $ nearCell world x

main = do
    (_progName, _args) <- getArgsAndInitialize
    _window <- createWindow "Life Game"
    arg <- newIORef ([(0, 0),(0, 1),(0, -1),(1, 0),(-1, 0)] :: [(Double, Double)])

    keyboardMouseCallback $= Just (keyboardProc arg)
    displayCallback $= display arg

    mainLoop
 
display arg = do 
    clear [ColorBuffer]
    pointSize $= 10
    w <- readIORef arg
    renderPrimitive Points $
        mapM_ (\(x, y) -> vertex $ Vertex2 (x / 10) (y / 10)) w
    flush

keyboardProc arg ch state _ _
    | ch     == Char 'q' = exitWith ExitSuccess
    | state    == Down   = modifyIORef arg execute
    | otherwise          = return ()
