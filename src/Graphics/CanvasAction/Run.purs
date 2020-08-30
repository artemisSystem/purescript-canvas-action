module Graphics.CanvasAction.Run where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect.Class as Effect
import Graphics.Canvas (Context2D)
import Graphics.CanvasAction.Types (CanvasActionM, runAction)
import Prim.Row as Row
import Run (EFFECT, FProxy, Run, SProxy(..), case_, lift, on, runRec, interpret, send)

-- | Type alias for using `CanvasActionM` as an effect
type CANVAS = FProxy CanvasActionM

-- | Symbol proxy for "canvas"
_canvas ∷ SProxy "canvas"
_canvas = SProxy

-- | Lift a `CanvasActionM` into `Run` via the `canvas` label
liftCanvasAction ∷ ∀ r a. CanvasActionM a → Run (canvas ∷ CANVAS | r) a
liftCanvasAction = liftCanvasActionAt _canvas

-- | Lift a `CanvasActionM` into `Run` via the provided label
liftCanvasActionAt ∷
  ∀ s r1 r2
  . IsSymbol s ⇒ Row.Cons s CANVAS r1 r2
  ⇒ SProxy s → CanvasActionM ~> Run r2
liftCanvasActionAt = lift

runCanvas ∷
  ∀ r
  . Context2D
  → Run (effect ∷ EFFECT, canvas ∷ CANVAS | r) ~> Run (effect ∷ EFFECT | r)
runCanvas = runCanvasAt (SProxy ∷ _ "effect") _canvas

runCanvasAt ∷
  ∀ effect canvas r0 r1 r2
  . IsSymbol effect
  ⇒ IsSymbol canvas
  ⇒ Row.Cons effect EFFECT r0 r1
  ⇒ Row.Cons canvas CANVAS r1 r2
  ⇒ SProxy effect
  → SProxy canvas
  → Context2D
  → Run r2 ~> Run r1
runCanvasAt effect canvas ctx = interpret
  (on canvas (runAction ctx >>> lift effect) send)

-- | Runs a base `CanvasActionM` effect
runBaseCanvas ∷ Run (canvas ∷ CANVAS) ~> CanvasActionM
runBaseCanvas = runBaseCanvasAt _canvas

-- | Runs a base `CanvasActionM` effect at the provided label
runBaseCanvasAt ∷
  ∀ s r. IsSymbol s ⇒ Row.Cons s CANVAS () r ⇒ SProxy s → Run r ~> CanvasActionM
runBaseCanvasAt p = runRec (case_ # on p identity)

-- | Runs base `CanvasActionM` and `Effect` together as one effect
runBaseCanvas' ∷ Run (effect ∷ EFFECT, canvas ∷ CANVAS) ~> CanvasActionM
runBaseCanvas' = runBaseCanvasAt' (SProxy ∷ _ "effect") _canvas

-- | Runs base `CanvasActionM` and `Effect` together as one effect at the
-- | provided labels
runBaseCanvasAt' ∷
  ∀ effect canvas r1 r2
  . IsSymbol effect
  ⇒ IsSymbol canvas
  ⇒ Row.Cons effect EFFECT () r1
  ⇒ Row.Cons canvas CANVAS r1 r2
  ⇒ SProxy effect
  → SProxy canvas
  → Run r2 ~> CanvasActionM
runBaseCanvasAt' effect canvas = case_
  # on effect Effect.liftEffect
  # on canvas identity
  # runRec
