module Test.Run.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic (makeRect)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D)
import Graphics.CanvasAction (fillRect, filled)
import Graphics.CanvasAction.Run (CANVAS, runCanvas)
import Run (EFFECT, Run, runBaseEffect)
import Run.Except (FAIL, fail, runFail)
import Web.HTML (window)
import Web.HTML.Window (confirm)

getCtx ∷ String → Effect Context2D
getCtx id = getCanvasElementById id >>= case _ of
  Just canv → getContext2D canv
  Nothing → throw "No canvas"

main ∷ Effect Unit
main = do
  ctx ← getCtx "canvas"
  action
    # runFail >>> void
    # runCanvas ctx
    # runBaseEffect

action ∷ ∀ r. Run (canvas ∷ CANVAS, effect ∷ EFFECT, except ∷ FAIL | r) Unit
action = filled "#aaf" do
  w ← liftEffect window
  fillRect (makeRect  10.0  10.0 80.0 80.0)
  fillRect (makeRect 110.0 110.0 80.0 80.0)
  fail
    # unlessM (liftEffect $ confirm "Draw the rest of the drawing?" w)
  fillRect (makeRect  10.0 110.0 80.0 80.0)
  fillRect (makeRect 110.0  10.0 80.0 80.0)
