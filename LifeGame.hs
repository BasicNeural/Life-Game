import Graphics.UI.GLUT
import Data.List
import System.Exit
import Data.IORef
import Data.Time
import Control.Parallel
import Control.Exception
import qualified Data.Sequence              as S
import qualified Data.Foldable              as F
import qualified Data.ByteString.Lazy.Char8 as BS

pfold :: (a -> a -> a) -> [a] -> a
pfold _ [x] = x
pfold mappend xs = (ys `par` zs) `pseq` (ys `mappend` zs) where
  len = length xs
  (ys', zs') = splitAt (len `div` 2) xs
  ys = pfold mappend ys'
  zs = pfold mappend zs'

nearCell :: [(Float, Float)] -> (Float, Float) -> Int
nearCell world (x, y) = length $ filter isNear world
    where isNear (newX, newY) = dist < 2 && dist >= 1
              where dist = sqrt $ (x - newX)**2 + (y - newY)**2

execute :: [(Float, Float)] -> [(Float, Float)]
execute world = filter (willAlive (\x -> x == 2 || x == 3)) world 
                `union` filter (willAlive (==3) ) deadCell
    where deadCell = pfold union $ map (\(x, y) -> 
                    [(x-1,y),(x+1,y),(x,y-1),(x,y+1),(x-1,y-1),(x+1,y-1)
                    ,(x-1,y+1),(x-1,y-1)]) world
          willAlive f x = f $ nearCell world x

main :: IO ()
main = do

    rawdata <- fmap BS.lines $ BS.readFile "./seed.csv"

    let set = map (map BS.unpack . BS.split ',') rawdata

    let centerList = head set
    let seedList = tail set

    let (x,y) = ( (-1) * read (centerList !! 0) :: Float , read (centerList !! 1) :: Float )
    let seed = map (\(x,y)->(y,x)) . concat . map (\(x,ys) -> zip (iterate (\x->x) x) ys) . zip [0,-1..] 
                $ map (map fst . filter (\(_,x) -> not (x == "" || x == "\r")) . zip [0..]) seedList
    
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 720 720
    _window <- createWindow "Life Game"
    world <- newIORef $ map (\(x_,y_) -> (x + x_, y + y_)) (seed :: [(Float, Float)])
    time <- getCurrentTime
    tick <- newIORef time
    counter <- newIORef 0.0
    speed <- newIORef 1.0
    frame <- newIORef 0.0
    gen <- newIORef 0

    a <- readIORef world
    keyboardMouseCallback $= Just (keyboardProc speed)
    displayCallback $= display world speed gen
    idleCallback $= Just (idle display world tick speed counter frame gen)

    mainLoop
 
display worldRef speedRef genRef = do 
    clear [ColorBuffer]
    pointSize $= 8
    

    world <- readIORef worldRef
    speed <- readIORef speedRef
    gen <- readIORef genRef

    renderPrimitive Points $
        mapM_ (\(x, y) -> vertex $ Vertex2 (x / 40) (y / 40)) world
    
    rasterPos (Vertex2 0.5 (-0.98) :: Vertex2 Float)
    renderString Fixed8By13 $ "Speed : " ++ init (show (1 / speed))
    rasterPos (Vertex2 (-0.9) (-0.98) :: Vertex2 Float)
    renderString Fixed8By13 $ "Gen : " ++ (show gen)
    
    swapBuffers

idle display worldRef tickRef speedRef counterRef frameRef genRef = do
    tick <- readIORef tickRef
    curr <- getCurrentTime
    
    let diff = diffUTCTime curr tick

    writeIORef tickRef (curr)
    modifyIORef counterRef (+diff)
    modifyIORef frameRef (+diff)

    speed <- readIORef speedRef
    counter <- readIORef counterRef
    frame <- readIORef frameRef

    if speed <= counter then do
        modifyIORef' counterRef ((-) speed)
        modifyIORef' worldRef execute
        modifyIORef' genRef (+1)
    else
        return ()

    if frame > 0.0167 then do
        modifyIORef' frameRef ((-) 0.0167)
        display worldRef speedRef genRef
    else
        return ()

keyboardProc speedRef ch state _ _
    | ch == Char 'q' = exitWith ExitSuccess
    | ch == Char 'a' && state == Down = setSpeed (< 64.0) (* 2.0)
    | ch == Char 'd' && state == Down = setSpeed (> 0.015625) (/ 2.0)
    | otherwise      = return ()
        where setSpeed limit f = do 
                speed <- readIORef speedRef
                if limit speed then
                    modifyIORef' speedRef f
                else
                    return ()
