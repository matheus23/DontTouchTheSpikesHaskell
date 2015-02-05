module DTTS.TypesAndConstants where

import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form

import Data.Vec2
import System.Random

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

canDie :: Bool
canDie = True

playerWidth :: Double
playerWidth = 0.3

playerHeight :: Double
playerHeight = 0.3

playerJumpVel :: Double
playerJumpVel = -0.15

playerMoveVel :: Int -> Double
playerMoveVel score = 0.1 + (fromIntegral score * 0.0005)

playerGravity :: Double
playerGravity = 0.008

spikeNumber :: Int -> Int
spikeNumber score = min (score `div` 5 + roundedPercentageOf 0.3 spikesVert) (roundedPercentageOf 0.7 spikesVert)

roundedPercentageOf :: Double -> Int -> Int
roundedPercentageOf percentage number = round $ percentage * fromIntegral number

spikesHoriz :: Int
spikesHoriz = 7

spikesVert :: Int
spikesVert = 11

spikePadding :: Double
spikePadding = 0.2

gameWidth :: Double
gameWidth = fromIntegral spikesHoriz + spikePadding * 2

gameHeight :: Double
gameHeight = fromIntegral spikesVert


colorsFromScore :: Int -> (RGB, RGB)
colorsFromScore score = colors !! (max 0 (min (length colors - 1) (score `div` 5)))

modulate :: Double -> RGB -> RGB -> RGB
modulate t (r1, g1, b1) (r2, g2, b2) = (r1*k + r2*t, g1*k + g2*t, b1*k + b2*t) where k = 1-t

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
  , ((0, 105, 233),   (1, 37, 131))
  , ((235, 1, 100),   (130, 0, 60))
  , ((255, 255, 255), (255, 171, 52))
  , ((0, 106, 236),   (0, 37, 132))
  , ((236, 0, 100),   (132, 0, 62))
  , ((255, 255, 255), (0, 162, 255))
  , ((255, 255, 255), (165, 52, 254)) ]
