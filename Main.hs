module Main where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape
import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Gtk.KeyboardInput

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import Data.List
import FRP.Behaviour
import System.Random

main :: IO ()
main = runFormBehaviour (0.5, 0.5) gameBehaviour

windowSize :: Behaviour GtkEvent (Int, Int)
windowSize = filterMapE filterSize $ remember (500, 500)
  where
    filterSize (Resize size) = Just size
    filterSize _ = Nothing

gameBehaviour :: Behaviour GtkEvent Form
gameBehaviour = merge scaleToSize (mapValue toVec2 windowSize) (mapValue (padded 10 . renderScreen) screensBehaviour)
  where toVec2 (x, y) = (fromIntegral x, fromIntegral y)

renderScreen :: Screen -> Form
renderScreen (PreGame gs) = pressKeySign `atop` (scaleToSizeOnAxis 500 Vec2.down $ render gs)
  where
    pressKeySign = signText "Press Any Key"
    signText str = centeredHV $ text defaultTextStyle { textColor = lightBlue, fontSize = 30, bold = True } str
renderScreen (InGame gs) = scaleToSizeOnAxis 500 Vec2.down $ render gs
renderScreen (GameOver gs) = (centeredHV $ append Vec2.down [gameOverSign, scoreSign]) `atop` (scaleToSizeOnAxis 500 Vec2.down $ render gs)
  where
    textStyle = defaultTextStyle { textColor = red, fontSize = 30, bold = True }
    gameOverSign = centeredHV $ text textStyle "Game Over"
    scoreSign = centeredHV $ text textStyle $ "Score: " ++ show (score gs)

screensBehaviour :: Behaviour GtkEvent Screen
screensBehaviour = fold (PreGame $ initialGS $ mkStdGen 42) simulateScreen
  where
    simulateScreen (KeyPress _) (PreGame gs)  = InGame gs
    simulateScreen _            (PreGame gs)  = PreGame gs
    simulateScreen (KeyPress _) (InGame gs)   = handleGameOver $ advanceGameState True gs
    simulateScreen Tick         (InGame gs)   = handleGameOver $ advanceGameState False gs
    simulateScreen (KeyPress _) (GameOver gs) = PreGame $ initialGS $ rand gs
    simulateScreen _ screen = screen

    handleGameOver (gs, True) = GameOver gs
    handleGameOver (gs, False) = InGame gs

data Screen = PreGame GameState | InGame GameState | GameOver GameState

data GameState = GameState
  { playerPos :: Vec2
  , playerVel :: Vec2
  , spikes :: [Int]
  , spikesBefore :: [Int]
  , score :: Int
  , rand :: StdGen
  , lastScoreTime :: Int
  }

advanceGameState :: Bool -> GameState -> (GameState, Bool)
advanceGameState up oldGS = (if outside newPlayerPos then addScoreGameState else flyoutGameOver) oldGS
    { playerPos = newPlayerPos
    , playerVel = newPlayerVel
    , lastScoreTime = lastScoreTime oldGS + 1 }
  where
    outside (x, y) = x <= playerWidth || x >= (gameWidth-playerWidth)
    newPlayerPos = Vec2.add (playerPos oldGS) (playerVel oldGS)
    newPlayerVel = if up then (fst $ playerVel oldGS, -0.15) else Vec2.add (playerVel oldGS) (0, 0.008)
    flyoutGameOver gs = (gs, y < playerHeight || y > gameHeight-playerHeight) where y = snd newPlayerPos

addScoreGameState :: GameState -> (GameState, Bool)
addScoreGameState gs = (gs
    { playerPos = turnPlayerPos $ playerPos gs
    , playerVel = turnPlayerVel $ playerVel gs
    , spikesBefore = spikes gs
    , spikes = newSpikes
    , score = score gs + 1
    , rand = newRand
    , lastScoreTime = 0 },
    gameOver $ playerPos gs)
  where
    (newSpikes, newRand) = genSpikes (rand gs) (score gs)
    turnPlayerPos (xpos, y)
      | xpos <= playerWidth = (playerWidth + playerWidth - xpos, y)
      | xpos >= (gameWidth-playerWidth) = (gameWidth-playerWidth-(gameWidth-playerWidth-xpos), y)
      | otherwise = (xpos, y)
    turnPlayerVel (xvel, y) = (-xvel, y)
    gameOver (_, y) = touchesSpike y
    touchesSpike y = or $ map (\x -> elem x $ spikes gs) [floor (y-playerHeight+0.1)..floor (y+playerHeight-0.1)]

initialGS :: StdGen -> GameState
initialGS rand = GameState (3.5, 5.5) (0.1, -0.15) spikes [] 0 rand' 0
  where (spikes, rand') = genSpikes rand 0

genSpikes :: StdGen -> Int -> ([Int], StdGen)
genSpikes rand score = (spikes, newRand)
  where (spikes, newRand) = uniqueRandoms rand (0, 10) $ min (score `div` 5 + 3) 7

uniqueRandoms :: (Eq a, Random a) => StdGen -> (a, a) -> Int -> ([a], StdGen)
uniqueRandoms rand range 0 = ([], rand)
uniqueRandoms rand range n = (uniqs ++ newUniqs, newRand)
  where
    uniqs = nub rands
    (rands, rand') = nRandoms rand range n
    (newUniqs, newRand) = uniqueRandoms rand' range (n - length uniqs)

nRandoms :: Random a => StdGen -> (a, a) -> Int -> ([a], StdGen)
nRandoms rand range 0 = ([], rand)
nRandoms rand range n = (oneRandomNumber : otherRandomNumbers, lastRandomGen)
  where
    (oneRandomNumber, newRand) = randomR range rand
    (otherRandomNumbers, lastRandomGen) = nRandoms newRand range (n-1)

playerWidth :: Double
playerWidth = 0.3

playerHeight :: Double
playerHeight = 0.3

gameWidth :: Double
gameWidth = 7.4

gameHeight :: Double
gameHeight = 11

render :: GameState -> Form
render gs = moveToCenter player `atop` scoreIndicator fg gs `atop` moveToCenter mainGame `atop` collapseBorder background
  where
    moveToCenter = move (-gameWidth/2, -gameHeight/2)

    ticks = lastScoreTime gs
    animTime = if ticks == 0 then 0 else min 1 (fromIntegral ticks / 15)
    (bgNow, fgNow) = colorsFromScore $ score gs
    (bgB4, fgB4) = colorsFromScore (score gs - 1)
    fg = modulate animTime fgB4 fgNow
    bg = modulate animTime bgB4 bgNow

    background = filled bg $ rectangle 100 100
    foreground = alignHV (0, 0) $ filled fg $ rectangle gameWidth gameHeight

    player = move (playerPos gs) $ filled red $ rectangle (playerWidth*2) (playerHeight*2)
    mainGame = spikesLeftRight `atop` topSpikeLine `atop` bottomSpikeLine `atop` foreground
    spikesLeftRight = spikeLineInfront `atop` spikeLineBehind

    spike = filled bg $ noBorder $ closedPath spikePath
    spikePath =
      pathPoint (-1, 0.1) `lineConnect`
      pathPoint (0, 0.1) `lineConnect`
      pathPoint (0.45, 0.5) `lineConnect`
      pathPoint (0, 0.9) `lineConnect`
      pathPoint (-1, 0.9)
    renderSpike pos = move (0, fromIntegral pos) spike
    renderSpikeLine line = foldl atop empty $ map renderSpike line
    renderOnSide True form = form
    renderOnSide False form = move (gameWidth, 0) $ scale (-1, 1) form
    spikeLineInfront = renderOnSide (odd $ score gs) $ move (-(1-animTime) * 0.45, 0) $ renderSpikeLine $ spikes gs
    spikeLineBehind = renderOnSide (even $ score gs) $ move (-animTime * 0.45, 0) $ renderSpikeLine $ spikesBefore gs
    spikeLine = rotate (Vec2.degrees (-90)) $ foldl atop empty $ map renderSpike [0..6]
    topSpikeLine = move (0.2, 0) $ scale (1, -1) spikeLine
    bottomSpikeLine = move (0.2, gameHeight) $ spikeLine

colorsFromScore :: Int -> (RGB, RGB)
colorsFromScore score = colors !! (max 0 (min (length colors - 1) (score `div` 5)))

modulate :: Double -> RGB -> RGB -> RGB
modulate t (r1, g1, b1) (r2, g2, b2) = (r1*k + r2*t, g1*k + g2*t, b1*k + b2*t) where k = 1-t

printCol :: RGB -> String
printCol (r, g, b) = show (roundDigits 2 r, roundDigits 2 g, roundDigits 2 b)
  where roundDigits n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

colors :: [(RGB, RGB)]
colors = map (\(bg, fg) -> (fromRGB bg, fromRGB fg))
  [ ((128, 128, 128), (235, 235, 235))
  , ((99, 117, 128),  (222, 234, 240))
  , ((128, 106, 99),  (244, 232, 225))
  , ((116, 128, 99),  (232, 241, 222))
  , ((107, 99, 128),  (230, 225, 244))
  , ((255, 255, 255), (114, 114, 114))
  , ((0, 190, 236),   (0, 106, 132))
  , ((128, 236, 0),   (40, 132, 0))
  , ((255, 255, 255), (255, 171, 52))
  , ((0, 106, 236),   (0, 37, 132))
  , ((236, 0, 100),   (132, 0, 62))
  , ((255, 255, 255), (0, 162, 255)) ]


scoreIndicator :: RGB -> GameState -> Form
scoreIndicator col gs = (centeredHV $ scaleToSizeOnAxis 3.5 Vec2.right $ scoreText $ score gs) `atop` filled white (circle 2)
  where
    scoreStyle = defaultTextStyle { textColor = col, fontSize = 8, fontFamily = "monospace", bold = True }
    scoreText score = text scoreStyle $ if length (show score) == 1 then '0' : show score else show score

scaleToSize :: (Double, Double) -> Form -> Form
scaleToSize (wantedX, wantedY) form = scale (minFac, minFac) form
  where
    minFac = min factorX factorY
    factorX = wantedX / graphicWidth form
    factorY = wantedY / graphicHeight form

scaleToSizeOnAxis :: Double -> Vec2 -> Form -> Form
scaleToSizeOnAxis wantedSize axis form = scale (factor, factor) form
  where
    factor = wantedSize / actualSize
    actualSize = Vec2.magnitude $ Border.borderSpanOnAxis (getBorder form) axis

runFormBehaviour :: (Double, Double) -> Behaviour GtkEvent Form -> IO ()
runFormBehaviour align behaviour = do
  runFormProgram align behaviour step
    where step input currentBehaviour = do
            let newBehaviour = runEvent currentBehaviour input
            return (newBehaviour, value $ newBehaviour)
