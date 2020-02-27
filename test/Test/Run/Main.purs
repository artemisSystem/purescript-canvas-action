module Test.Run.Main where

import Prelude

import Data.Maybe (fromJust)
import Data.Vector.Polymorphic (makeRect)
import Effect (Effect)
import Effect.Class (liftEffect)
import Graphics.Canvas (getCanvasElementById, getContext2D, Context2D)
import Graphics.CanvasAction (fillRect, filled, asEffect)
import Graphics.CanvasAction.Run (CANVAS, runBaseCanvas')
import Partial.Unsafe (unsafePartial)
import Run (EFFECT, Run)
import Run.Except (FAIL, fail, runFail)
import Web.HTML (window)
import Web.HTML.Window (confirm)

-- will crash if the canvas doesn't exist
getCtx :: String -> Effect Context2D
getCtx id = getCanvasElementById id <#> unsafePartial fromJust >>= getContext2D

main :: Effect Unit
main = do
  ctx <- getCtx "canvas"
  action
    # runFail >>> void
    # asEffect ctx runBaseCanvas'

action
  :: forall r. Run (canvas :: CANVAS, effect :: EFFECT, except :: FAIL | r) Unit
action = filled "#aaf" do
  w <- liftEffect window
  fillRect (makeRect  10.0  10.0 80.0 80.0)
  fillRect (makeRect 110.0 110.0 80.0 80.0)
  fail
    # unlessM (liftEffect $ confirm "Draw the rest of the drawing?" w)
  fillRect (makeRect  10.0 110.0 80.0 80.0)
  fillRect (makeRect 110.0  10.0 80.0 80.0)
