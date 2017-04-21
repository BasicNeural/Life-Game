import Graphics.UI.GLUT
import Data.List
import System.Exit
import Data.IORef
import Data.Time
import Control.Exception

timerInterval = 17

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
    world <- newIORef ([(-37,33),(-37,32),(-36,33),(-36,32),(-27,33),(-27,32)
                       ,(-27,31),(-26,34),(-26,30),(-25,35),(-25,29),(-24,35)
                       ,(-24,29),(-23,32),(-22,34),(-22,30),(-21,33),(-21,32)
                       ,(-21,31),(-20,32),(-17,35),(-17,34),(-17,33),(-16,35)
                       ,(-16,34),(-16,33),(-15,36),(-15,32),(-13,37),(-13,36)
                       ,(-13,32),(-13,31),(-3,35),(-3,34),(-2,35),(-2,34)]
                       :: [(Double, Double)])
    time <- getCurrentTime
    tick <- newIORef time
    counter <- newIORef 0.0
    speed <- newIORef 1.0

    keyboardMouseCallback $= Just (keyboardProc speed)
    displayCallback $= display world tick speed counter
    addTimerCallback timerInterval $ timerProc (display world tick speed counter)

    mainLoop
 
display worldRef tickRef speedRef counterRef = do 
    clear [ColorBuffer]
    pointSize $= 8
    tick <- readIORef tickRef
    curr <- getCurrentTime
    
    let diff = diffUTCTime curr tick

    writeIORef tickRef (curr)
    modifyIORef counterRef (+diff)

    speed <- readIORef speedRef
    counter <- readIORef counterRef

    if speed <= counter then do
        modifyIORef counterRef ((-) speed)
        modifyIORef worldRef execute
    else
        return ()

    world <- readIORef worldRef

    renderPrimitive Points $
        mapM_ (\(x, y) -> vertex $ Vertex2 (x / 40) (y / 40)) world
    
    rasterPos (Vertex2 0.5 (-0.98) :: Vertex2 Float)
    renderString Fixed8By13 $ "Speed : " ++ show speed
    
    flush

timerProc act = do
    act
    addTimerCallback timerInterval $ timerProc act

keyboardProc speed ch state _ _
    | ch == Char 'q' = exitWith ExitSuccess
    | ch == Char 'a' && state == Down = modifyIORef speed (*2.0)
    | ch == Char 'd' && state == Down = modifyIORef speed (/2.0)
    | otherwise      = return ()