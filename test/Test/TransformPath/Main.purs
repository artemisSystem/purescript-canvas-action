module Test.TransformPath.Main where

import Prelude

import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic (Rect(..), Vector2, (><))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.CanvasAction (class CanvasStyle, class MonadCanvasAction, Context2D, filled, getCanvasElementById, getContext2D, launchCanvasAff_, setGlobalAlpha, setLineWidth)
import Graphics.CanvasAction.Path (FillRule(..), PathAction, arcBy_, fill, moveTo, runPath, stroke)
import Graphics.CanvasAction.Transformation (rotate, transform, transformedBy, translate)
import Math (tau)
import Test.ScaleForDPR (scaleForDPR)

centeredRect ∷ Vector2 Number → Vector2 Number → Rect Number
centeredRect center size = Rect pos size
  where
  pos = center - size / pure 2.0

roundedRectangle ∷ Rect Number → Number → PathAction Unit
roundedRectangle (Rect (x >< y) (width >< height)) radius = do
  moveTo (x + radius >< y)
  arcBy_ (width - radius >< 0.0) (0.0 >< radius) radius
  arcBy_ (0.0 >< height - radius) (-radius >< 0.0) radius
  arcBy_ (-width + radius >< 0.0) (0.0 >< -radius) radius
  arcBy_ (0.0 >< -height + radius) (radius >< 0.0) radius

drawRoundedRectangle
  ∷ ∀ m color
  . CanvasStyle color
  ⇒ MonadCanvasAction m
  ⇒ color
  → Rect Number
  → Number
  → m Unit
drawRoundedRectangle color rect radius = do
  path ← runPath (roundedRectangle rect radius)
  filled color (fill Nonzero path)
  stroke path

setup ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
setup = do
  setLineWidth 2.0
  setGlobalAlpha 0.7
  transform (translate 200.0 200.0)

action ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
action = forWithIndex_ colors \i color → do
  let size = pure (300.0 - toNumber i * 50.0)
  transformedBy (rotate (tau / 30.0 * toNumber i)) do
    drawRoundedRectangle color (centeredRect zero size) 20.0
  where
  colors = [ "#faa", "#ffa", "#afa", "#aff", "#aaf", "#faf" ]

getCtx ∷ String → Effect Context2D
getCtx id = getCanvasElementById id >>= case _ of
  Just canv → getContext2D canv
  Nothing → throw "No canvas"

main ∷ Effect Unit
main = do
  ctx ← getCtx "canvas"
  launchCanvasAff_ ctx do
    scaleForDPR (400.0 >< 400.0) (setup *> action)
