import Graphics.UI.GLUT
import Data.List
import System.Exit
import Data.IORef

nearCell world (x, y) = length $ filter isNear world
    where isNear (new_x, new_y) = dist < 2 && dist >= 1
              where dist = sqrt $ (x - new_x)**2 + (y - new_y)**2

execute world = filter (\(x,y) -> abs x < 40 && abs y < 40)
              $ filter (willAlive (\x -> x == 2 || x == 3)) world 
                `union` filter (willAlive (==3) ) deadCell
    where deadCell = foldl1' union $ map (\(x, y) -> 
                    [(x-1,y),(x+1,y),(x,y-1),(x,y+1),(x-1,y-1),(x+1,y-1)
                    ,(x-1,y+1),(x-1,y-1)]) world
          willAlive f x = f $ nearCell world x

main = do
    (_progName, _args) <- getArgsAndInitialize
    initialWindowSize $= Size 720 720
    _window <- createWindow "Life Game"
    arg <- newIORef ([(-37,33),(-37,32),(-36,33),(-36,32),(-27,33),(-27,32)
                     ,(-27,31),(-26,34),(-26,30),(-25,35),(-25,29),(-24,35)
                     ,(-24,29),(-23,32),(-22,34),(-22,30),(-21,33),(-21,32)
                     ,(-21,31),(-20,32),(-17,35),(-17,34),(-17,33),(-16,35)
                     ,(-16,34),(-16,33),(-15,36),(-15,32),(-13,37),(-13,36)
                     ,(-13,32),(-13,31),(-3,35),(-3,34),(-2,35),(-2,34)]
                     :: [(Double, Double)])

    keyboardMouseCallback $= Just (keyboardProc arg)
    displayCallback $= display arg

    mainLoop
 
display arg = do 
    clear [ColorBuffer]
    pointSize $= 8
    w <- readIORef arg
    renderPrimitive Points $
        mapM_ (\(x, y) -> vertex $ Vertex2 (x / 40) (y / 40)) w
    flush

keyboardProc arg ch state _ _
    | ch == Char 'q' = exitWith ExitSuccess
    | state == Down  = modifyIORef arg execute >> display arg
    | otherwise      = return ()