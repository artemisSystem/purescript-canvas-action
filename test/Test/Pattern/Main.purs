module Test.Pattern.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic (makeRect, (><))
import Effect (Effect)
import Effect.Exception (throw)
import Graphics.CanvasAction (class MonadCanvasAction, Context2D, PatternRepeat(..), createPattern, fillRect, fillRectFull, filled, getCanvasElementById, getContext2D, imageSource, launchCanvasAff_, runActionOffscreen, setImageSmoothing)
import Graphics.CanvasAction.Path (FillRule(..), circle, fillWith, runPath)
import Test.ScaleForDPR (scaleForDPR)

setup ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
setup = setImageSmoothing false

action ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
action = do
  pattern ← runActionOffscreen (20.0 >< 20.0) do
    filled "#aaf" fillRectFull
    filled "#afa" $ fillRect (makeRect 0.0 10.0 10.0 10.0)
    filled "#faa" $ fillRect (makeRect 10.0 0.0 10.0 10.0)
    imageSource >>= (_ `createPattern` Repeat)
  (fillWith pattern Nonzero <=< runPath) do
    circle (200.0 >< 200.0) 175.0
    circle (50.0 >< 50.0) 50.0
    circle (50.0 >< 350.0) 50.0
    circle (350.0 >< 50.0) 50.0
    circle (350.0 >< 350.0) 50.0

getCtx ∷ String → Effect Context2D
getCtx id = getCanvasElementById id >>= case _ of
  Just canv → getContext2D canv
  Nothing → throw "No canvas"

main ∷ Effect Unit
main = do
  ctx ← getCtx "canvas"
  launchCanvasAff_ ctx do
    scaleForDPR (400.0 >< 400.0) (setup *> action)
