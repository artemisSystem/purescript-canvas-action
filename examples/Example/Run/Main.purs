module Example.Run.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic (makeRect, (><))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D)
import Graphics.CanvasAction (fillRect, launchCanvasAff_, setFillStyle)
import Graphics.CanvasAction.Run (CTX, runBaseCanvasAff')
import Run (EFFECT, Run)
import Run.Except (FAIL, fail, runFail)
import Example.ScaleForDPR (scaleForDPR)
import Type.Row (type (+))
import Web.HTML (window)
import Web.HTML.Window (confirm)

getCtx ∷ String → Effect Context2D
getCtx id = getCanvasElementById id >>= case _ of
  Just canv → getContext2D canv
  Nothing → throw "No canvas"

action ∷ ∀ r. Run (CTX + EFFECT + FAIL + r) Unit
action = do
  w ← liftEffect window
  setFillStyle "#aaf"
  fillRect (makeRect 10.0 10.0 80.0 80.0)
  fillRect (makeRect 110.0 110.0 80.0 80.0)
  fail
    # unlessM (liftEffect $ confirm "Draw the rest of the drawing?" w)
  setFillStyle "#faa"
  fillRect (makeRect 10.0 110.0 80.0 80.0)
  fillRect (makeRect 110.0 10.0 80.0 80.0)

main ∷ Effect Unit
main = do
  ctx ← getCtx "canvas"
  scaleForDPR (200.0 >< 200.0) action
    # runFail >>> void
    -- Aff comes from `scaleForDPR`
    # runBaseCanvasAff'
    # launchCanvasAff_ ctx
