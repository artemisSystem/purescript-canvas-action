module Graphics.CanvasAction.Types where

import Control.Monad.Reader.Trans (ReaderT(..))
import Effect (Effect)
import Graphics.Canvas (Context2D)
import Prelude (Unit)

-- | The `CanvasActionM` monad is a monad transformer stack, more specifically
-- | the `ReaderT` monad transformer applied to the `Effect` monad, reading a 
-- | value of type `Context2D`.
type CanvasActionM = ReaderT Context2D Effect

-- | Type synonym for a `CanvasActionM` without a result
type CanvasAction = CanvasActionM Unit


-- | Run a `CanvasActionM` in the `Effect` monad, on the provided `Context2D`
runAction :: forall a. Context2D -> CanvasActionM a -> Effect a
runAction ctx (ReaderT action) = action ctx