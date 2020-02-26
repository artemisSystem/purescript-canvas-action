module Graphics.CanvasAction.Class where

import Prelude

import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Writer.Trans (WriterT)
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Graphics.Canvas (Context2D)
import Graphics.CanvasAction.Types (CanvasActionM)
import Graphics.CanvasAction.Run (CANVAS)
import Run (EFFECT, Run)
import Run.Internal (fromRows)
import Type.Equality (class TypeEquals)
import Type.Row (RProxy)

-- | The `MonadCanvasAction` class captures those monads which support canvas
-- | actions.
-- |
-- | Instances are provided for `CanvasActionM` itself, and the standard monad
-- | transformers.
-- |
-- | `liftCanvasAction` can be used in any appropriate monad transformer stack
-- | to lift an action of type `CanvasActionM a` into the monad.
class (MonadEffect m) <= MonadCanvasAction m where
  liftCanvasAction :: CanvasActionM ~> m

instance monadCanvasActionCanvasActionM :: MonadCanvasAction (ReaderT Context2D Effect) where
  liftCanvasAction = identity

else instance monadCanvasActionReaderT :: MonadCanvasAction m => MonadCanvasAction (ReaderT r m) where
  liftCanvasAction = lift <<< liftCanvasAction


instance monadCanvasActionRun :: (TypeEquals (RProxy r1) (RProxy (canvas :: CANVAS, effect :: EFFECT | r2))) ⇒ MonadCanvasAction (Run r1) where
  liftCanvasAction = fromRows <<< liftCanvasAction


instance monadCanvasActionContT :: MonadCanvasAction m => MonadCanvasAction (ContT r m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionExceptT :: MonadCanvasAction m => MonadCanvasAction (ExceptT e m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionListT :: MonadCanvasAction m => MonadCanvasAction (ListT m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionMaybeT :: MonadCanvasAction m => MonadCanvasAction (MaybeT m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionRWST :: (Monoid w, MonadCanvasAction m) => MonadCanvasAction (RWST r w s m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionStateT :: MonadCanvasAction m => MonadCanvasAction (StateT s m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionWriterT :: (Monoid w, MonadCanvasAction m) => MonadCanvasAction (WriterT w m) where
  liftCanvasAction = lift <<< liftCanvasAction
