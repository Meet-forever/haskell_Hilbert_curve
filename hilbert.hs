type Canvas = ([(Int,Int)],Int,Int)
pixel = 20

canvasSize :: Int -> Int
canvasSize i
    | i <= 0 = 0
    | otherwise = (2^i * pixel) - pixel

-- Failed Idea:
-- | otherwise = (i*i*pixel) - (pixel*(i-1))

-- Down : South, Up: North, Right: East, Left: West
-- Down : 20, Up: -20, Right: 20, Left: -20

east  :: Canvas -> Canvas
east (l, x, y) = ((x+pixel, y):l , x+pixel, y) 
west  :: Canvas -> Canvas
west (l, x, y) = ((x-pixel, y):l , x-pixel, y) 
north :: Canvas -> Canvas
north (l, x, y) = ((x, y-pixel):l , x, y-pixel) 
south :: Canvas -> Canvas
south (l, x, y) = ((x, y+pixel):l , x, y+pixel) 


-- Curve :: Int -> Canvas -> Canvas

hCurve :: Int -> Canvas -> Canvas
hCurve i c
    | i == 0 = c
    | otherwise =  (bCurve (i-1) . north . hCurve (i-1) . east . hCurve (i-1) . south . aCurve (i-1)) c


aCurve :: Int -> Canvas -> Canvas
aCurve i c
    | i == 0 = c
    | otherwise =  (cCurve (i-1) . west . aCurve (i-1) . south . aCurve (i-1) . east . hCurve (i-1)) c


bCurve :: Int -> Canvas -> Canvas
bCurve i c
    | i == 0 = c
    | otherwise =  (hCurve (i-1) . east . bCurve (i-1) . north . bCurve (i-1) . west . cCurve (i-1)) c


cCurve :: Int -> Canvas -> Canvas
cCurve i c
    | i == 0 = c
    | otherwise =  (aCurve (i-1) . south . cCurve (i-1) . west . cCurve (i-1) . north . bCurve (i-1)) c