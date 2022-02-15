module Graphics.CanvasAction.Class
  ( class MonadCanvasAction
  , liftCanvasAction
  , class MonadCanvasAff
  , liftCanvasAff
  ) where

import Prelude

import Control.Monad.Cont.Trans (ContT)
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS.Trans (RWST)
import Control.Monad.Reader.Trans (ReaderT, mapReaderT)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Trans (WriterT)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.CanvasAction.Run (CTX)
import Graphics.CanvasAction.Run as CRun
import Graphics.CanvasAction.Types (CanvasAction, CanvasAff)
import Run (EFFECT, Run, AFF)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

fromRows' ∷ ∀ f r1 r2 a. TypeEquals (Proxy r1) (Proxy r2) ⇒ f r2 a → f r1 a
fromRows' = unsafeCoerce

-- | The `MonadCanvasAction` class captures those monads which support canvas
-- | actions.
-- |
-- | Instances are provided for `CanvasAction`, `CanvasAff`, and the standard
-- | monad transformers.
-- |
-- | `liftCanvasAction` can be used in any appropriate monad transformer stack
-- | to lift an action of type `CanvasAction a` into the monad.
class MonadEffect m ⇐ MonadCanvasAction m where
  liftCanvasAction ∷ CanvasAction ~> m

instance monadCanvasActionCanvasAction ∷ MonadCanvasAction CanvasAction where
  liftCanvasAction = identity

else instance monadCanvasActionCanvasAff ∷ MonadCanvasAction CanvasAff where
  liftCanvasAction = mapReaderT liftEffect

else instance monadCanvasActionReaderT ∷
  MonadCanvasAction m ⇒
  MonadCanvasAction (ReaderT r m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionRun ∷
  ( TypeEquals (Proxy r1) (Proxy (CTX + EFFECT + r2))
  ) ⇒
  MonadCanvasAction (Run r1) where
  liftCanvasAction = fromRows' <<< CRun.liftCanvasAction

instance monadCanvasActionContT ∷
  MonadCanvasAction m ⇒
  MonadCanvasAction (ContT r m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionExceptT ∷
  MonadCanvasAction m ⇒
  MonadCanvasAction (ExceptT e m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionListT ∷
  MonadCanvasAction m ⇒
  MonadCanvasAction (ListT m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionMaybeT ∷
  MonadCanvasAction m ⇒
  MonadCanvasAction (MaybeT m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionRWST ∷
  ( Monoid w
  , MonadCanvasAction m
  ) ⇒
  MonadCanvasAction (RWST r w s m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionStateT ∷
  MonadCanvasAction m ⇒
  MonadCanvasAction (StateT s m) where
  liftCanvasAction = lift <<< liftCanvasAction

instance monadCanvasActionWriterT ∷
  ( Monoid w
  , MonadCanvasAction m
  ) ⇒
  MonadCanvasAction (WriterT w m) where
  liftCanvasAction = lift <<< liftCanvasAction

-- | The `MonadCanvasAff` class captures those monads which support canvas
-- | actions and `Aff`s.
-- |
-- | Instances are provided for `CanvasAff` and the standard monad transformers.
-- |
-- | `liftCanvasAff` can be used in any appropriate monad transformer stack
-- | to lift an action of type `CanvasAff a` into the monad.
class (MonadAff m, MonadCanvasAction m) ⇐ MonadCanvasAff m where
  liftCanvasAff ∷ CanvasAff ~> m

instance monadCanvasAffCanvasAff ∷ MonadCanvasAff CanvasAff where
  liftCanvasAff = identity

else instance monadCanvasAffReaderT ∷
  MonadCanvasAff m ⇒
  MonadCanvasAff (ReaderT r m) where
  liftCanvasAff = lift <<< liftCanvasAff

instance monadCanvasAffRun ∷
  ( TypeEquals (Proxy r1) (Proxy (CTX + AFF + EFFECT + r2))
  ) ⇒
  MonadCanvasAff (Run r1) where
  liftCanvasAff = fromRows' <<< CRun.liftCanvasAff

instance monadCanvasAffContT ∷ MonadCanvasAff m ⇒ MonadCanvasAff (ContT r m) where
  liftCanvasAff = lift <<< liftCanvasAff

instance monadCanvasAffExceptT ∷ MonadCanvasAff m ⇒ MonadCanvasAff (ExceptT e m) where
  liftCanvasAff = lift <<< liftCanvasAff

instance monadCanvasAffListT ∷ MonadCanvasAff m ⇒ MonadCanvasAff (ListT m) where
  liftCanvasAff = lift <<< liftCanvasAff

instance monadCanvasAffMaybeT ∷ MonadCanvasAff m ⇒ MonadCanvasAff (MaybeT m) where
  liftCanvasAff = lift <<< liftCanvasAff

instance monadCanvasAffRWST ∷
  ( Monoid w
  , MonadCanvasAff m
  ) ⇒
  MonadCanvasAff (RWST r w s m) where
  liftCanvasAff = lift <<< liftCanvasAff

instance monadCanvasAffStateT ∷ MonadCanvasAff m ⇒ MonadCanvasAff (StateT s m) where
  liftCanvasAff = lift <<< liftCanvasAff

instance monadCanvasAffWriterT ∷
  ( Monoid w
  , MonadCanvasAff m
  ) ⇒
  MonadCanvasAff (WriterT w m) where
  liftCanvasAff = lift <<< liftCanvasAff
