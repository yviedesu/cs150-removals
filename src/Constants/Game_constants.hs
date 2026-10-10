module Constants.Game_constants where

gridRows :: Int
gridRows = 13

gridCols :: Int
gridCols = 15

tileSize :: Int
tileSize = 40

canvasWidth :: Int
canvasWidth = gridCols * tileSize

canvasHeight :: Int
canvasHeight = gridRows * tileSize

bombRange :: Int
bombRange = 1

playerSpeed :: Double
playerSpeed = 0.16

bombTimerMs :: Double
bombTimerMs = 3000

explosionDurationMs :: Double
explosionDurationMs = 1000

gameTimerDurationS :: Double
gameTimerDurationS = 60

maxActiveBombs :: Int
maxActiveBombs = 1

playerSize :: Double
playerSize = 30

spawnCell :: (Int, Int)
spawnCell = (1, 1)

keyUp :: Int
keyUp = 38

keyDown :: Int
keyDown = 40

keyLeft :: Int
keyLeft = 37

keyRight :: Int
keyRight = 39

keySpace :: Int
keySpace = 32
