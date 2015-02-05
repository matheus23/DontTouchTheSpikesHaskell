module DTTS.Rendering where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered
import qualified Graphics.Declarative.Border as Border
import Graphics.Declarative.Cairo.TangoColors
import Graphics.Declarative.Cairo.Form
import Graphics.Declarative.Cairo.Shape

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import DTTS.TypesAndConstants

renderScreen :: Screen -> Form
renderScreen (PreGame gs) = pressKeySign `atop` (scaleToSizeOnAxis 500 Vec2.down $ render gs)
  where
    pressKeySign = signText "Press Any Key"
    signText str = centeredHV $ text defaultTextStyle { textColor = lightBlue, fontSize = 30, bold = True } str
renderScreen (InGame gs) = scaleToSizeOnAxis 500 Vec2.down $ render gs
renderScreen (Pause gs) = scaleToSizeOnAxis 500 Vec2.down $ render gs
renderScreen (GameOver gs) = (centeredHV $ append Vec2.down [gameOverSign, scoreSign]) `atop` (scaleToSizeOnAxis 500 Vec2.down $ render gs)
  where
    textStyle = defaultTextStyle { textColor = red, fontSize = 30, bold = True }
    gameOverSign = centeredHV $ text textStyle "Game Over"
    scoreSign = centeredHV $ text textStyle $ "Score: " ++ show (score gs)

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

    background = filled bg $ rectangle (gameWidth*10) (gameHeight*10)
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
    spikeLine = rotate (Vec2.degrees (-90)) $ foldl atop empty $ map renderSpike [0..spikesHoriz-1]
    topSpikeLine = move (spikePadding, 0) $ scale (1, -1) spikeLine
    bottomSpikeLine = move (spikePadding, gameHeight) $ spikeLine


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
