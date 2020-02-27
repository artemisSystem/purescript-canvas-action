module Graphics.CanvasAction.Run where

import Prelude

import Data.Symbol (class IsSymbol)
import Effect.Class as Effect
import Graphics.CanvasAction.Types (CanvasActionM)
import Prim.Row as Row
import Run (EFFECT, FProxy, Run, SProxy(..), case_, lift, on, runRec)

-- | Type alias for using `CanvasActionM` as an effect
type CANVAS = FProxy CanvasActionM

-- | Symbol proxy for "canvas"
_canvas :: SProxy "canvas"
_canvas = SProxy

-- | Lift a `CanvasActionM` into `Run` via the `canvas` label
liftCanvasAction :: forall r a. CanvasActionM a -> Run (canvas :: CANVAS | r) a
liftCanvasAction = liftCanvasActionAt _canvas

-- | Lift a `CanvasActionM` into `Run` via the provided label
liftCanvasActionAt
  :: forall s r1 r2
   . IsSymbol s => Row.Cons s CANVAS r1 r2
  => SProxy s -> CanvasActionM ~> Run r2
liftCanvasActionAt = lift

-- | Runs a base `CanvasActionM` effect
runBaseCanvas :: Run (canvas :: CANVAS) ~> CanvasActionM
runBaseCanvas = runBaseCanvasAt _canvas

-- | Runs a base `CanvasActionM` effect at the provided label
runBaseCanvasAt
  :: forall s r
   . IsSymbol s => Row.Cons s CANVAS () r
  => SProxy s -> Run r ~> CanvasActionM
runBaseCanvasAt p = runRec (case_ # on p identity)

-- | Runs base `CanvasActionM` and `Effect` together as one effect
runBaseCanvas' :: Run (canvas :: CANVAS, effect :: EFFECT) ~> CanvasActionM
runBaseCanvas' = runBaseCanvasAt' (SProxy :: _ "effect") _canvas

-- | Runs base `CanvasActionM` and `Effect` together as one effect at the
-- | provided labels
runBaseCanvasAt'
  :: forall effect canvas r1 r2
   . IsSymbol effect
  => IsSymbol canvas
  => Row.Cons effect EFFECT () r1
  => Row.Cons canvas CANVAS r1 r2
  => SProxy effect
  -> SProxy canvas
  -> Run r2
  ~> CanvasActionM
runBaseCanvasAt' effect canvas = case_
  # on effect Effect.liftEffect
  # on canvas identity
  # runRec