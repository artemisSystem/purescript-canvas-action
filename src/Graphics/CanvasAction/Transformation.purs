-- | This module defines a free monad for working with canvas transformations in
-- | a more pleasant way. Construct transforms with the various functions, like
-- | `translate`, `scale` and `matrixTransform`. There are also alternate
-- | functions (denoted by a `'`), which take the dedicated data type for that
-- | transform, instead of `Number`s, as arguments. Perform a transform with
-- | `runTransform`, or use `transformed` to also reset the transformation
-- | matrix to its previous value afterwards.

module Graphics.CanvasAction.Transformation
  ( TransformationF
  , TransformationM
  , Transformation

  , resetTransform
  , setTransform
  , setTransform'
  , getTransform
  , translate
  , translate'
  , scale
  , scale'
  , skew
  , skew'
  , rotate
  , matrixTransform
  , matrixTransform'

  , runTransform
  , transformed
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Graphics.Canvas (TranslateTransform, ScaleTransform)
import Graphics.CanvasAction (class MonadCanvasAction, MatrixTransform, SkewTransform)
import Graphics.CanvasAction as CA


data TransformationF a
  = ResetTransform                   a
  | SetTransform  MatrixTransform    a
  | GetTransform (MatrixTransform  → a)
  | Translate     TranslateTransform a
  | Scale         ScaleTransform     a
  | Skew          SkewTransform      a
  | Rotate        Number             a
  | Matrix        MatrixTransform    a

derive instance functorTransformationF ∷ Functor TransformationF

type TransformationM = Free TransformationF
type Transformation = TransformationM Unit


-- | Reset the transformation matrix to its default value
resetTransform ∷ Transformation
resetTransform = liftF $ ResetTransform unit

-- | Construct a `Transformation` from a `MatrixTransform`, replacing the
-- | current matrix
setTransform' ∷ MatrixTransform → Transformation
setTransform' f = liftF $ SetTransform f unit

-- | Construct a `Transformation` from six `Number`s representing a matrix
-- | transformation, replacing the current matrix
setTransform ∷
  Number → Number → Number → Number → Number → Number → Transformation
setTransform a b c d e f =
  setTransform' { a, b, c, d, e, f }

-- | Get the current transformation matrix
getTransform ∷ TransformationM MatrixTransform
getTransform = liftF $ GetTransform identity

-- | Construct a `Transformation` from a `TranslateTransform`
translate' ∷ TranslateTransform → Transformation
translate' t = liftF $ Translate t unit

-- | Construct a `Transformation` from two `Number`s representing translation
translate ∷ Number → Number → Transformation
translate translateX translateY = translate' { translateX, translateY }

-- | Construct a `Transformation` from a `ScaleTransform`
scale' ∷ ScaleTransform → Transformation
scale' s = liftF $ Scale s unit

-- | Construct a `Transformation` from two `Number`s representing scaling
scale ∷ Number → Number → Transformation
scale scaleX scaleY = scale' { scaleX, scaleY }

-- | Construct a `Transformation` from a `SkewTransform`
skew' ∷ SkewTransform → Transformation
skew' s = liftF $ Skew s unit

-- | Construct a `Transformation` from two `Number`s representing skew
skew ∷ Number → Number → Transformation
skew skewX skewY = skew' { skewX, skewY }

-- | Construct a `Transformation` from a `Number` representing radians rotated
rotate ∷ Number → Transformation
rotate rad = liftF $ Rotate rad unit

-- | Construct a `Transformation` from a `MatrixTransform`, multiplying the
-- | new matrix with the current matrix
matrixTransform' ∷ MatrixTransform → Transformation
matrixTransform' f = liftF $ Matrix f unit

-- | Construct a `Transformation` from six `Number`s representing a matrix
-- | transformation, multiplying the new matrix with the current matrix
matrixTransform ∷
  Number → Number → Number → Number → Number → Number → Transformation
matrixTransform a b c d e f =
  matrixTransform' { a, b, c, d, e, f }

-- | Run a transformation
runTransform ∷ ∀ m. MonadCanvasAction m ⇒ MonadRec m ⇒ TransformationM ~> m
runTransform = runFreeM go
  where
    go (ResetTransform a) = CA.resetTransform_    $> a
    go (SetTransform f a) = CA.setTransform_    f $> a
    go (GetTransform   f) = CA.getTransform_     <#> f
    go (Translate    t a) = CA.translate_       t $> a
    go (Scale        s a) = CA.scale_           s $> a
    go (Skew         s a) = CA.skew_            s $> a
    go (Rotate       n a) = CA.rotate_          n $> a
    go (Matrix       f a) = CA.matrixTransform_ f $> a

-- | Run a transformation on a `MonadCanvasAction`, transforming back afterwards
transformed ∷
  ∀ m a. MonadCanvasAction m ⇒ MonadRec m ⇒ Transformation → m a → m a
transformed t action = do
  old ← CA.getTransform_
  runTransform t *> action <* CA.setTransform_ old
