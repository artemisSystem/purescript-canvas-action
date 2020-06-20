module Test.TransformPath.Main where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic ((><))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.CanvasAction (CanvasAction, Context2D, getCanvasElementById, getContext2D, runAction, setGlobalAlpha, setLineWidth)
import Graphics.CanvasAction.Transformation (rotate, runTransform, scale, skew, transformed, translate)
import Graphics.CanvasAction.Path (Path, circle, fillPathWith, polygon, strokePathWith)
import Math (tau)


getCtx ∷ CanvasAction → String → Effect Context2D
getCtx setupCanvas id = getCanvasElementById id >>= case _ of
  Just canv → getContext2D canv >>= \ctx → runAction ctx setupCanvas $> ctx
  Nothing → throw "No canvas"

main ∷ Effect Unit
main = do
  ctx ← getCtx setup "canvas"
  runAction ctx (setup *> action)

path ∷ Path
path = do
  polygon
    [   0.0 ><   0.0
    , 100.0 ><   0.0
    , 200.0 >< 100.0
    , 150.0 >< 100.0
    ]
  circle (50.0 >< 50.0) 25.0
  circle (90.0 >< 75.0) 30.0

setup ∷ CanvasAction
setup = do
  setLineWidth 3.0
  setGlobalAlpha 0.8

action ∷ CanvasAction
action = transformed (translate 50.0 50.0) do
  transformed (translate (-25.0) (-10.0)) $ traverse_ (_ $ path)
    [ transformed (scale 2.0 2.0 <> skew 0.0 0.3) <<< fillPathWith "#6f9"
    , strokePathWith "#333"
    , fillPathWith "#6f6"
    ]
  runTransform (scale 1.5 1.1 <> translate 100.0 0.0)
  transformed (translate 50.0 0.0 <> rotate (tau/8.0)) do
    fillPathWith "#69f" path
  fillPathWith "#99f" path
