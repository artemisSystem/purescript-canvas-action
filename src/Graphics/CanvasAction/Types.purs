module Graphics.CanvasAction.Types where

import Prelude

import Control.Monad.Reader.Trans (ReaderT(..))
import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Graphics.Canvas (Context2D)

-- | The `CanvasActionM` monad is a monad transformer stack, more specifically
-- | the `ReaderT` monad transformer applied to the `Effect` monad, reading a
-- | value of type `Context2D`.
type CanvasActionM = ReaderT Context2D Effect

-- | Type synonym for a `CanvasActionM` without a result
type CanvasAction = CanvasActionM Unit

-- | Run a `CanvasActionM` in a `MonadEffect`, on the provided `Context2D`.
runAction :: forall m a. MonadEffect m => Context2D -> CanvasActionM a -> m a
runAction ctx (ReaderT action) = liftEffect (action ctx)