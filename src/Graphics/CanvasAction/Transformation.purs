-- | This module defines a free monad for working with canvas transformations in
-- | a more pleasant way. Construct transforms with the various functions, like
-- | `translate`, `scale` and `fullMatrix`. There are also alternate functions
-- | (denoted by a `'`), which take the dedicated data type for that transform,
-- | instead of `Number`s, as arguments. Perform a transform with
-- | `runTransform`, or use `transformed` to also reset the transformation
-- | matrix to its previous value afterwards.

module Graphics.CanvasAction.Transformation
  ( TransformationF
  , TransformationM
  , Transformation
  
  , translate
  , translate'
  , scale
  , scale'
  , skew
  , skew'
  , rotate
  , fullMatrix
  , fullMatrix'

  , runTransform
  , transformed
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Graphics.Canvas (TranslateTransform, ScaleTransform)
import Graphics.CanvasAction (class MonadCanvasAction, FullMatrixTransform, SkewTransform, getTransform, setTransform)
import Graphics.CanvasAction (translate, rotate, scale, fullMatrix, skew) as CA


data TransformationF a
  = Translate  TranslateTransform  a
  | Scale      ScaleTransform      a
  | Skew       SkewTransform       a
  | Rotate     Number              a
  | FullMatrix FullMatrixTransform a

derive instance functorTransformationF :: Functor TransformationF

type TransformationM = Free TransformationF
type Transformation = TransformationM Unit


-- | Construct a `Transformation` from a `TranslateTransform`
translate' :: TranslateTransform -> Transformation
translate' t = liftF $ Translate t unit

-- | Construct a `Transformation` from two `Number`s representing translation
translate :: Number -> Number -> Transformation
translate translateX translateY = translate' { translateX, translateY }

-- | Construct a `Transformation` from a `ScaleTransform`
scale' :: ScaleTransform -> Transformation
scale' s = liftF $ Scale s unit

-- | Construct a `Transformation` from two `Number`s representing scaling
scale :: Number -> Number -> Transformation
scale scaleX scaleY = scale' { scaleX, scaleY }

-- | Construct a `Transformation` from a `SkewTransform`
skew' :: SkewTransform -> Transformation
skew' s = liftF $ Skew s unit

-- | Construct a `Transformation` from two `Number`s representing skew
skew :: Number -> Number -> Transformation
skew skewX skewY = skew' { skewX, skewY }

-- | Construct a `Transformation` from a `Number` representing radians rotated
rotate :: Number -> Transformation
rotate rad = liftF $ Rotate rad unit

-- | Construct a `Transformation` from a `FullMatrixTransform`
fullMatrix' :: FullMatrixTransform -> Transformation
fullMatrix' f = liftF $ FullMatrix f unit

-- | Construct a `Transformation` from six `Number`s representing a full
-- | matrix transformation
fullMatrix
  :: Number -> Number -> Number -> Number -> Number -> Number -> Transformation
fullMatrix m11 m12 m21 m22 m31 m32 =
  fullMatrix' { m11, m12, m21, m22, m31, m32 }

-- | Run a transformation
runTransform
  :: forall m. MonadCanvasAction m => MonadRec m => TransformationM ~> m
runTransform = runFreeM go
  where
    go (Translate  t a) = CA.translate  t $> a
    go (Scale      s a) = CA.scale      s $> a
    go (Skew       s a) = CA.skew       s $> a
    go (Rotate     n a) = CA.rotate     n $> a
    go (FullMatrix f a) = CA.fullMatrix f $> a

-- | Run a transformation on a `MonadCanvasAction`, transforming back afterwards
transformed
  :: forall m a
   . MonadCanvasAction m => MonadRec m
  => Transformation -> m a -> m a
transformed t act = do
  old <- getTransform
  runTransform t *> act <* setTransform old