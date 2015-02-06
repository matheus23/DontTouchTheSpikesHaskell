module DTTS.Logic where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Gtk.Window
import Graphics.Declarative.Gtk.KeyboardInput

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import Data.List
import FRP.Behaviour
import System.Random

import DTTS.TypesAndConstants
import DTTS.Rendering

gameBehaviour :: Behaviour GtkEvent Form
gameBehaviour = merge scaleToSize (mapValue toVec2 windowSize) (mapValue (padded 10 . renderScreen) screensBehaviour)
  where toVec2 (x, y) = (fromIntegral x, fromIntegral y)

windowSize :: Behaviour GtkEvent (Int, Int)
windowSize = filterMapE filterSize $ remember (500, 500)
  where
    filterSize (Resize size) = Just size
    filterSize _ = Nothing

screensBehaviour :: Behaviour GtkEvent Screen
screensBehaviour = fold (PreGame $ initialGS $ mkStdGen 42) simulateScreen
  where
    simulateScreen (KeyPress (Special Escape)) (InGame gs)   = Pause gs
    simulateScreen (KeyPress (Special Escape)) (Pause gs)    = InGame gs
    simulateScreen (KeyPress _)                (PreGame gs)  = InGame gs
    simulateScreen _                           (PreGame gs)  = PreGame gs
    simulateScreen (KeyPress _)                (InGame gs)   = handleGameOver $ advanceGameState True gs
    simulateScreen Tick                        (InGame gs)   = handleGameOver $ advanceGameState False gs
    simulateScreen (KeyPress _)                (GameOver gs) = PreGame $ initialGS $ rand gs
    simulateScreen _ screen = screen

    handleGameOver (gs, True) = GameOver gs
    handleGameOver (gs, False) = InGame gs



initialGS :: StdGen -> GameState
initialGS rand = GameState (gameWidth/2, gameHeight/2) (playerMoveVel 0, playerJumpVel) [] [] 0 0

genSpikes :: StdGen -> Int -> ([Int], StdGen)
genSpikes rand score = (spikes, newRand)
  where (spikes, newRand) = uniqueRandoms rand (0, spikesVert-1) $ spikeNumber score

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

advanceGameState :: Bool -> GameState -> (GameState, Bool)
advanceGameState up oldGS = (if outside newPlayerPos then addScoreGameState else flyoutGameOver) oldGS
    { playerPos = newPlayerPos
    , playerVel = newPlayerVel
    , lastScoreTime = lastScoreTime oldGS + 1 }
  where
    outside (x, y) = x <= playerWidth || x >= (gameWidth-playerWidth)
    newPlayerPos = Vec2.add (playerPos oldGS) (playerVel oldGS)
    newPlayerVel = if up then (signum (fst $ playerVel oldGS) * playerMoveVel (score oldGS), playerJumpVel) else Vec2.add (playerVel oldGS) (0, playerGravity)
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
    canDie && gameOver (playerPos gs))
  where
    (newSpikes, newRand) = genSpikes (rand gs) (score gs)
    turnPlayerPos (xpos, y)
      | xpos <= playerWidth = (playerWidth + playerWidth - xpos, y)
      | xpos >= (gameWidth-playerWidth) = (gameWidth-playerWidth-(gameWidth-playerWidth-xpos), y)
      | otherwise = (xpos, y)
    turnPlayerVel (xvel, y) = (-xvel, y)
    gameOver (_, y) = touchesSpike y
    touchesSpike y = or $ map (\x -> elem x $ spikes gs) [floor (y-playerHeight+0.1)..floor (y+playerHeight-0.1)]
