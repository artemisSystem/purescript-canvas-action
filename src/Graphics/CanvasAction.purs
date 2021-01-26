-- | This module defines all functions from `Graphics.Canvas` (and some extra)
-- | as functions without the `Context2D` parameter, and in the `CanvasActionM`
-- | monad instead of the `Effect` monad. This is to make canvas actions easily
-- | composable without having to worry about passing the `Context2D` to every
-- | single function. They can be composed using `do` notation / bind or
-- | applicative composition (`<*>`, `<*`, and `*>`). Semigroup composition
-- | (`<>`) also works if the return types are the same and also a `Semigroup`
-- | (this includes `Unit`). It also has a `MonadRec` instance. Functions from
-- | this module can be used with any `MonadCanvasActionM`, which is a type
-- | class with instances for monads which support canvas actions. Instances are
-- | provided for `CanvasActionM`, `Run`, and the standard monad transformers.

module Graphics.CanvasAction
  ( SkewTransform
  , MatrixTransform
  , TextBaseline(..)

  , CanvasStyle
  , styleIsString
  , styleIsGradient
  , styleIsPattern
  , styleToString
  , styleToString'
  , styleToGradient
  , styleToGradient'
  , styleToPattern
  , styleToPattern'
  , class CanvasStyleRep
  , toStyle
  , class CanvasColorRep
  , toColor

  , createCanvas
  , createCanvas'
  , getCanvasElementById
  , querySelectCanvas
  , getContext2D
  , getContext2DById
  , querySelectContext2D

  , runActionOffscreen
  , runActionOffscreen'
  , asEffect

  , withCtx
  , withCtx1
  , withCtx2
  , withCtx3
  , withCtx4
  , withCtx5
  , withCtx6
  , withCtx7
  , withCtx8
  , withCtx9
  , withFull
  , withMidPos

  , getCanvasEffect
  , getCanvas
  , withCanvas
  , withCanvas1
  , withCanvas2
  , withCanvas3

  , fillRect
  , fillRectFull
  , strokeRect
  , strokeRectFull
  , clearRect
  , clearRectFull

  , setFillStyle
  , setStrokeStyle
  , getFillStyle
  , getStrokeStyle
  , filled
  , stroked

  , setLineWidth
  , setLineDash
  , setShadowBlur
  , setShadowOffsetX
  , setShadowOffsetY
  , setShadowOffset
  , setShadowColor
  , setMiterLimit
  , setLineCap
  , setLineJoin
  , setGlobalCompositeOperation
  , setGlobalAlpha

  , getTextAlign
  , setTextAlign
  , getTextBaseline
  , setTextBaseline
  , getFont
  , setFont
  , fillText
  , strokeText
  , measureText

  , dimensionsToSize
  , getDimensions
  , setDimensions
  , getHeight
  , setHeight
  , getWidth
  , setWidth
  , toDataUrl

  , getImageData
  , putImageDataFull
  , putImageData
  , createImageData
  , createImageDataCopy

  , imageSource
  , drawImage
  , drawImageScale
  , drawImageFull
  , tryLoadImage
  , tryLoadImage'
  , loadImageAff
  , setImageSmoothing
  , getImageSmoothing

  , createPattern

  , createLinearGradient
  , createRadialGradient
  , addColorStop
  , linearGradient
  , radialGradient

  , resetTransform_
  , setTransform_
  , getTransform_
  , translate_
  , scale_
  , skew_
  , rotate_
  , matrixTransform_

  , beginPath_
  , stroke_
  , fill_
  , clip_
  , lineTo_
  , moveTo_
  , closePath_
  , arc_
  , rect_
  , quadraticCurveTo_
  , bezierCurveTo_

  , save
  , restore
  , restoreAfter

  , module Exports
  ) where

import Prelude

import Color (Color, cssStringRGBA)
import Control.Monad.Reader (ReaderT(..), ask)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, for_)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, effectCanceler)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Exception (error)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref as Ref
import Graphics.Canvas (Arc, BezierCurve, CanvasElement, CanvasGradient, CanvasImageSource, CanvasPattern, Composite, Context2D, Dimensions, ImageData, LineCap, LineJoin, LinearGradient, PatternRepeat, QuadraticCurve, RadialGradient, ScaleTransform, TextAlign, TextMetrics, TranslateTransform)
import Graphics.Canvas (Arc, BezierCurve, CanvasElement, CanvasGradient, CanvasImageSource, CanvasPattern, Composite(..), Context2D, Dimensions, ImageData, LineCap(..), LineJoin(..), LinearGradient, PatternRepeat(..), QuadraticCurve, RadialGradient, ScaleTransform, TextAlign(..), TextMetrics, TranslateTransform, imageDataBuffer, imageDataHeight, imageDataWidth) as Exports
import Graphics.CanvasAction.Class (class MonadCanvasAction, liftCanvasAction)
import Graphics.CanvasAction.Class (class MonadCanvasAction, liftCanvasAction) as Exports
import Graphics.CanvasAction.Types (CanvasAction, CanvasActionM, runAction)
import Graphics.CanvasAction.Types (CanvasAction, CanvasActionM, runAction) as Exports
import Graphics.Canvas as C
import Data.Vector.Polymorphic (Rect(..), Vector2, midPos, toRectangle, (><))
import Data.Vector.Polymorphic.Class (class FromSize, class ToPos, class ToRegion, class ToSize, fromSize, toPos, toRegion, toSize)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.DOM.Document (Document, createElement)
import Web.DOM.ParentNode (QuerySelector, querySelector)
import Web.DOM.ParentNode (QuerySelector(..)) as Exports
import Web.HTML.HTMLCanvasElement (fromElement, HTMLCanvasElement)
import Web.HTML.HTMLDocument (toDocument, toParentNode) as HTMLDocument
import Web.HTML (window)
import Web.HTML.Window (document)


-- | Type synonym for skew transformations.
type SkewTransform = { skewX ∷ Number, skewY ∷ Number }

-- | Type synonym for matrix transformations.
-- This is the same as `Graphics.Canvas`'s `Transform`, but there is a separate
-- type in this module to avoid confusion with `Transformation` from
-- `Graphics.CanvasAction.Transformation`.
-- NOTE: Currently, `Graphics.Canvas` have these fields noted as `m11` to `m32`,
-- but this is inconsistent with the `CanvasRenderingContext2D` API.
-- MatrixTransform is correct.
type MatrixTransform =
  { a ∷ Number
  , b ∷ Number
  , c ∷ Number
  , d ∷ Number
  , e ∷ Number
  , f ∷ Number
  }

-- | Enumerates types of text baseline
data TextBaseline
  = BaselineTop
  | BaselineHanging
  | BaselineMiddle
  | BaselineAlphabetic
  | BaselineIdeographic
  | BaselineBottom

instance showTextBaseline ∷ Show TextBaseline where
  show BaselineTop = "BaselineTop"
  show BaselineHanging = "BaselineHanging"
  show BaselineMiddle = "BaselineMiddle"
  show BaselineAlphabetic = "BaselineAlphabetic"
  show BaselineIdeographic = "BaselineIdeographic"
  show BaselineBottom = "BaselineBottom"

-- | A value that can be passed to `setFillStyle` and similar functions.
-- | Runtime representation should be either a `String`, a `CanvasGradient` or
-- | a `CanvasPattern`.
foreign import data CanvasStyle ∷ Type

foreign import styleIsString ∷ CanvasStyle → Boolean

foreign import styleIsGradient ∷ CanvasStyle → Boolean

foreign import styleIsPattern ∷ CanvasStyle → Boolean

unsafeStyleToX ∷ ∀ a. (CanvasStyle → Boolean) → (CanvasStyle → Maybe a)
unsafeStyleToX isCorrect style
  | isCorrect style = Just (unsafeCoerce style)
  | otherwise       = Nothing

unsafeStyleToX' ∷ ∀ a. (CanvasStyle → Boolean) → (Partial ⇒ CanvasStyle → a)
unsafeStyleToX' isC style = fromJust $ unsafeStyleToX isC style

styleToString ∷ CanvasStyle → Maybe String
styleToString = unsafeStyleToX styleIsString

styleToString' ∷ Partial ⇒ CanvasStyle → String
styleToString' = unsafeStyleToX' styleIsString

styleToGradient ∷ CanvasStyle → Maybe CanvasGradient
styleToGradient = unsafeStyleToX styleIsGradient

styleToGradient' ∷ Partial ⇒ CanvasStyle → CanvasGradient
styleToGradient' = unsafeStyleToX' styleIsGradient

styleToPattern ∷ CanvasStyle → Maybe CanvasPattern
styleToPattern = unsafeStyleToX styleIsPattern

styleToPattern' ∷ Partial ⇒ CanvasStyle → CanvasPattern
styleToPattern' = unsafeStyleToX' styleIsPattern


-- | Class describing types that can be turned into a valid `CanvasStyle` for
-- | use with `setFillStyle` and similar functions. This way, there is no
-- | need for functions like `setPatternFillStyle`, **and** values of types like
-- | `Color` can easily used as a fillStyle without problem.
class CanvasStyleRep rep where
  toStyle ∷ rep → CanvasStyle

instance canvasStyleRepCanvasStyle ∷ CanvasStyleRep CanvasStyle where
  toStyle = identity

instance canvasStyleRepString ∷ CanvasStyleRep String where
  toStyle = unsafeCoerce

instance canvasStyleRepColor ∷ CanvasStyleRep Color where
  toStyle = toStyle <<< cssStringRGBA

instance canvasStyleRepGradient ∷ CanvasStyleRep CanvasGradient where
  toStyle = unsafeCoerce

instance canvasStyleRepPattern ∷ CanvasStyleRep CanvasPattern where
  toStyle = unsafeCoerce

-- | Class describing types that can be turned into a string representing a
-- | canvas color.
class CanvasColorRep rep where
  toColor ∷ rep → String

instance canvasColorRepString ∷ CanvasColorRep String where
  toColor = identity

instance canvasColorRepColor ∷ CanvasColorRep Color where
  toColor = cssStringRGBA

createCanvas_ ∷ Document → Effect CanvasElement
createCanvas_ = createElement "canvas" >>> map unsafeCoerceElem
  where unsafeCoerceElem ∷ Element → CanvasElement
        unsafeCoerceElem = unsafeCoerce

-- | Same as `createCanvasEffect`, but allows for specifying the `Document`
-- | object to create the canvas with
createCanvasEffect'
  ∷ ∀ s. ToSize Number s ⇒ Document → s → Effect CanvasElement
createCanvasEffect' doc = toSize >>> \(width >< height) → do
  canvas ← createCanvas_ doc
  C.setCanvasDimensions canvas { width, height }
  pure canvas

-- | Create a `CanvasElement` of the given size in the `Effect` monad
createCanvasEffect ∷ ∀ s. ToSize Number s ⇒ s → Effect CanvasElement
createCanvasEffect s = do
  doc ← window >>= document
  createCanvasEffect' (HTMLDocument.toDocument doc) s

-- | Same as `createCanvas`, but allows for specifying the `Document` object to
-- | create the canvas with
createCanvas' ∷
  ∀ m s
  . MonadEffect m ⇒ ToSize Number s
  ⇒ Document → s → m CanvasElement
createCanvas' doc s = liftEffect $ createCanvasEffect' doc s

-- | Create a `CanvasElement` of the given size in any `MonadCanvasAction`
createCanvas ∷ ∀ m s . MonadEffect m ⇒ ToSize Number s ⇒ s → m CanvasElement
createCanvas s = liftEffect $ createCanvasEffect s

getCanvasElementById ∷ ∀ m. MonadEffect m ⇒ String → m (Maybe CanvasElement)
getCanvasElementById = liftEffect <<< C.getCanvasElementById

querySelectCanvas ∷ ∀ m. MonadEffect m ⇒ QuerySelector → m (Maybe CanvasElement)
querySelectCanvas canvas = liftEffect do
  doc ← window >>= document <#> HTMLDocument.toParentNode
  querySelector canvas doc
    <#> (=<<) fromElement
    <#> map toCanvasElement
  where
    toCanvasElement ∷ HTMLCanvasElement → C.CanvasElement
    toCanvasElement = unsafeCoerce

getContext2D ∷ ∀ m. MonadEffect m ⇒ CanvasElement → m Context2D
getContext2D = liftEffect <<< C.getContext2D

getContext2DById ∷ ∀ m. MonadEffect m ⇒ String → m (Maybe Context2D)
getContext2DById = getCanvasElementById >=> traverse getContext2D

querySelectContext2D ∷ ∀ m. MonadEffect m ⇒ QuerySelector → m (Maybe Context2D)
querySelectContext2D = querySelectCanvas >=> traverse getContext2D


{-
| Run a `CanvasActionM` in a `MonadEffect`, on a created canvas with the
| provided size. This can be useful for creating patterns for use as a
| fillStyle or strokeStyle.
|
| For example:
|
| ```purescript
| action ∷ CanvasAction
| action = do
|   pattern ← runActionOffscreen (20.0 >< 20.0) do
|     filled "#aaf" fillRectFull
|     filled "#afa" $ fillRect (makeRect 0.0 10.0 10.0 10.0)
|     filled "#faa" $ fillRect (makeRect 10.0 0.0 10.0 10.0)
|     imageSource >>= (_ `createPattern` Repeat)
|   fillPathWith pattern do
|     circle (200.0 >< 200.0) 175.0
|     circle ( 50.0 ><  50.0)  50.0
|     circle ( 50.0 >< 350.0)  50.0
|     circle (350.0 ><  50.0)  50.0
|     circle (350.0 >< 350.0)  50.0
| ```
-}
runActionOffscreen ∷
  ∀ m a s
  . MonadEffect m
  ⇒ ToSize Number s ⇒ s → CanvasActionM a → m a
runActionOffscreen size action = do
  ctx ← createCanvas size >>= getContext2D
  runAction ctx action

-- | Run a `CanvasActionM` in a `MonadCanvasAction`, on a created canvas with
-- | the same size as the "main" canvas. This can be useful for creating
-- | patterns for use as a fillStyle or strokeStyle. See `runActionOffscreen`
-- | for an example.
runActionOffscreen' ∷ ∀ m a. MonadCanvasAction m ⇒ CanvasActionM a → m a
runActionOffscreen' action = do
  (size ∷ Vector2 Number) ← getDimensions <#> dimensionsToSize
    # liftCanvasAction
  runActionOffscreen size action

-- | Runs a `CanvasActionM` in a `MonadEffect` inside a functor context. Useful
-- | for turning a function that returns a `CanvasActionM` into a function that
-- | returns an `Effect`.
asEffect
  ∷ ∀ m f a
  . Functor f ⇒ MonadEffect m
  ⇒ Context2D → f (CanvasActionM a) → f (m a)
asEffect ctx = map (runAction ctx)

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with no arguments apart from the
-- | `Context2D`.
withCtx ∷ ∀ m a . MonadCanvasAction m ⇒ (Context2D → Effect a) → m a
withCtx = liftCanvasAction <<< ReaderT

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with one argument apart from the
-- | `Context2D`.
withCtx1 ∷ ∀ m a b. MonadCanvasAction m ⇒ (Context2D → a → Effect b) → (a → m b)
withCtx1 action a = withCtx \ctx → action ctx a

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with two arguments apart from the
-- | `Context2D`.
withCtx2 ∷
  ∀ m a b c
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → Effect c)
  → (a → b → m c)
withCtx2 action a b = withCtx \ctx → action ctx a b

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with three arguments apart from the
-- | `Context2D`.
withCtx3 ∷
  ∀ m a b c d
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → c → Effect d)
  → (a → b → c → m d)
withCtx3 action a b c = withCtx \ctx → action ctx a b c

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with four arguments apart from the
-- | `Context2D`.
withCtx4 ∷
  ∀ m a b c d e
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → c → d → Effect e)
  → (a → b → c → d → m e)
withCtx4 action a b c d = withCtx \ctx → action ctx a b c d

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with five arguments apart from the
-- | `Context2D`.
withCtx5 ∷
  ∀ m a b c d e f
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → c → d → e → Effect f)
  → (a → b → c → d → e → m f)
withCtx5 action a b c d e = withCtx \ctx → action ctx a b c d e

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with six arguments apart from the
-- | `Context2D`.
withCtx6 ∷
  ∀ m a b c d e f g
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → c → d → e → f → Effect g)
  → (a → b → c → d → e → f → m g)
withCtx6 action a b c d e f = withCtx \ctx → action ctx a b c d e f

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with seven arguments apart from the
-- | `Context2D`.
withCtx7 ∷
  ∀ m a b c d e f g h
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → c → d → e → f → g → Effect h)
  → (a → b → c → d → e → f → g → m h)
withCtx7 action a b c d e f g = withCtx \ctx → action ctx a b c d e f g

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with eight arguments apart from the
-- | `Context2D`.
withCtx8 ∷
  ∀ m a b c d e f g h i
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → c → d → e → f → g → h → Effect i)
  → (a → b → c → d → e → f → g → h → m i)
withCtx8 action a b c d e f g h = withCtx \ctx → action ctx a b c d e f g h

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with nine arguments apart from the
-- | `Context2D`.
withCtx9 ∷
  ∀ m a b c d e f g h i j
  . MonadCanvasAction m
  ⇒ (Context2D → a → b → c → d → e → f → g → h → i → Effect j)
  → (a → b → c → d → e → f → g → h → i → m j)
withCtx9 action a b c d e f g h i = withCtx \ctx → action ctx a b c d e f g h i

-- | From a function taking some region and returning a `CanvasActionM`, make
-- | a `CanvasActionM` that calls the original function with the whole canvas
-- | as the region. This can for example be used to draw an image scaled to fill
-- | the entire canvas: `withFull \r → drawImageScale r image`
withFull ∷ ∀ m a. MonadCanvasAction m ⇒ (∀ r. ToRegion Number r ⇒ r → m a) → m a
withFull action = liftCanvasAction (getDimensions <#> dimsToVector2) >>= action
  where dimsToVector2 = dimensionsToSize ∷ Dimensions → Vector2 Number

-- | From a function taking some position and returning a `CanvasActionM`, make
-- | a `CanvasActionM` that calls the original function with the center of the
-- | canvas as a position.
withMidPos ∷
  ∀ m a
  . MonadCanvasAction m
  ⇒ (∀ p. ToPos Number p ⇒ p → m a)
  → m a
withMidPos action = liftCanvasAction (getDimensions <#> midPos') >>= action
  where
    dimsToVector2 = dimensionsToSize ∷ Dimensions → Vector2 Number
    midPos' = dimsToVector2 >>> midPos ∷ Dimensions → Vector2 Number

-- | Get the canvas of a `Context2D`
foreign import getCanvasEffect ∷ Context2D → Effect CanvasElement

-- | Get the canvas as a `CanvasActionM`
getCanvas ∷ ∀ m. MonadCanvasAction m ⇒ m CanvasElement
getCanvas = withCtx getCanvasEffect

-- | Convenience function for constructing `CanvasActionM`s from
-- | `Graphics.Canvas`-style functions with no arguments apart from the
-- | `CanvasElement`.
withCanvas ∷ ∀ m a. MonadCanvasAction m ⇒ (CanvasElement → Effect a) → m a
withCanvas action = withCtx \ctx → (getCanvasEffect ctx >>= action)

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with one argument apart from the
-- | `CanvasElement`.
withCanvas1 ∷
  ∀ m a b. MonadCanvasAction m ⇒ (CanvasElement → a → Effect b) → (a → m b)
withCanvas1 action a = withCanvas \canv → action canv a

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with two arguments apart from the
-- | `CanvasElement`.
withCanvas2 ∷
  ∀ m a b c
  . MonadCanvasAction m
  ⇒ (CanvasElement → a → b → Effect c)
  → (a → b → m c)
withCanvas2 action a b = withCanvas \canv → action canv a b

-- | Convenience function for constructing `MonadCanvasAction`s from
-- | `Graphics.Canvas`-style functions with three arguments apart from the
-- | `CanvasElement`.
withCanvas3 ∷
  ∀ m a b c d
  . MonadCanvasAction m
  ⇒ (CanvasElement → a → b → c → Effect d)
  → (a → b → c → m d)
withCanvas3 action a b c = withCanvas \canv → action canv a b c

-- | Fill a rectangular area
fillRect ∷ ∀ m r. MonadCanvasAction m ⇒ ToRegion Number r ⇒ r → m Unit
fillRect = withCtx1 C.fillRect <<< toRectangle

-- | Fill a rectangular area that covers the entire canvas
fillRectFull ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
fillRectFull = withFull fillRect

-- | Stroke a rectangular area
strokeRect ∷ ∀ m r. MonadCanvasAction m ⇒ ToRegion Number r ⇒ r → m Unit
strokeRect = withCtx1 C.strokeRect <<< toRectangle

-- | Stroke a rectangular area that covers the entire canvas
strokeRectFull ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
strokeRectFull = withFull strokeRect

-- | Clear a rectangular area
clearRect ∷ ∀ m r. MonadCanvasAction m ⇒ ToRegion Number r ⇒ r → m Unit
clearRect = withCtx1 C.clearRect <<< toRectangle

-- | Clear a rectangular area that covers the entire canvas
clearRectFull ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
clearRectFull = withFull clearRect


foreign import setFillStyleImpl ∷ Context2D → CanvasStyle → Effect Unit

setFillStyle ∷ ∀ m r. MonadCanvasAction m ⇒ CanvasStyleRep r ⇒ r → m Unit
setFillStyle = withCtx1 setFillStyleImpl <<< toStyle

foreign import setStrokeStyleImpl ∷ Context2D → CanvasStyle → Effect Unit

setStrokeStyle ∷ ∀ m r. MonadCanvasAction m ⇒ CanvasStyleRep r ⇒ r → m Unit
setStrokeStyle = withCtx1 setStrokeStyleImpl <<< toStyle

foreign import getFillStyleImpl ∷ Context2D → Effect CanvasStyle

getFillStyle ∷ ∀ m. MonadCanvasAction m ⇒ m CanvasStyle
getFillStyle = withCtx getFillStyleImpl

foreign import getStrokeStyleImpl ∷ Context2D → Effect CanvasStyle

getStrokeStyle ∷ ∀ m. MonadCanvasAction m ⇒ m CanvasStyle
getStrokeStyle = withCtx getStrokeStyleImpl

-- | Run a `CanvasActionM` with the given fillStyle, resetting it to the
-- | previous value after
filled ∷ ∀ m a r. MonadCanvasAction m ⇒ CanvasStyleRep r ⇒ r → m a → m a
filled style action = do
  old ← getFillStyle
  setFillStyle style *> action <* setFillStyle old

-- | Run a `CanvasActionM` with the given strokeStyle, resetting it to the
-- | previous value after
stroked ∷ ∀ m a r. MonadCanvasAction m ⇒ CanvasStyleRep r ⇒ r → m a → m a
stroked style action = do
  old ← getStrokeStyle
  setStrokeStyle style *> action <* setStrokeStyle old

setLineWidth ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setLineWidth = withCtx1 C.setLineWidth

setLineDash ∷ ∀ m. MonadCanvasAction m ⇒ Array Number → m Unit
setLineDash = withCtx1 C.setLineDash

setShadowBlur ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setShadowBlur = withCtx1 C.setShadowBlur

setShadowOffsetX ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setShadowOffsetX = withCtx1 C.setShadowOffsetX

setShadowOffsetY ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setShadowOffsetY = withCtx1 C.setShadowOffsetY

-- | Set x and y shadow offset at the same time
setShadowOffset ∷ ∀ m p. MonadCanvasAction m ⇒ ToPos Number p ⇒ p → m Unit
setShadowOffset offset = setShadowOffsetX x *> setShadowOffsetY y
  where (x >< y) = toPos offset

setShadowColor ∷ ∀ m r. MonadCanvasAction m ⇒ CanvasColorRep r ⇒ r → m Unit
setShadowColor = withCtx1 C.setShadowColor <<< toColor

setMiterLimit ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setMiterLimit = withCtx1 C.setMiterLimit

setLineCap ∷ ∀ m. MonadCanvasAction m ⇒ LineCap → m Unit
setLineCap = withCtx1 C.setLineCap

setLineJoin ∷ ∀ m. MonadCanvasAction m ⇒ LineJoin → m Unit
setLineJoin = withCtx1 C.setLineJoin

setGlobalCompositeOperation ∷ ∀ m. MonadCanvasAction m ⇒ Composite → m Unit
setGlobalCompositeOperation = withCtx1 C.setGlobalCompositeOperation

setGlobalAlpha ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setGlobalAlpha = withCtx1 C.setGlobalAlpha


getTextAlign ∷ ∀ m. MonadCanvasAction m ⇒ m TextAlign
getTextAlign = withCtx C.textAlign

setTextAlign ∷ ∀ m. MonadCanvasAction m ⇒ TextAlign → m Unit
setTextAlign = withCtx1 C.setTextAlign

foreign import getTextBaselineImpl ∷ Context2D → Effect String

getTextBaseline ∷ ∀ m. MonadCanvasAction m ⇒ m TextBaseline
getTextBaseline =
  withCtx \ctx → unsafeParseBaseline <$> getTextBaselineImpl ctx
    where
      unsafeParseBaseline "top" = BaselineTop
      unsafeParseBaseline "hanging" = BaselineHanging
      unsafeParseBaseline "middle" = BaselineMiddle
      unsafeParseBaseline "alphabetic" = BaselineAlphabetic
      unsafeParseBaseline "ideographic" = BaselineIdeographic
      unsafeParseBaseline "bottom" = BaselineBottom
      unsafeParseBaseline baseline = unsafeThrow $
        "Invalid TextBaseline: " <> baseline

foreign import setTextBaselineImpl ∷ Context2D → String → Effect Unit

setTextBaseline ∷ ∀ m. MonadCanvasAction m ⇒ TextBaseline → m Unit
setTextBaseline baseline = withCtx1 setTextBaselineImpl (toString baseline)
  where
    toString BaselineTop = "top"
    toString BaselineHanging = "hanging"
    toString BaselineMiddle = "middle"
    toString BaselineAlphabetic = "alphabetic"
    toString BaselineIdeographic = "ideographic"
    toString BaselineBottom = "bottom"


getFont ∷ ∀ m. MonadCanvasAction m ⇒ m String
getFont = withCtx C.font

setFont ∷ ∀ m. MonadCanvasAction m ⇒ String → m Unit
setFont = withCtx1 C.setFont

fillText ∷ ∀ m p. MonadCanvasAction m ⇒ ToPos Number p ⇒ String → p → m Unit
fillText text pos = withCtx3 C.fillText text x y
  where (x >< y) = toPos pos

strokeText ∷ ∀ m p. MonadCanvasAction m ⇒ ToPos Number p ⇒ String → p → m Unit
strokeText text pos = withCtx3 C.strokeText text x y
  where (x >< y) = toPos pos

measureText ∷ ∀ m. MonadCanvasAction m ⇒ String → m TextMetrics
measureText = withCtx1 C.measureText


dimensionsToSize ∷ ∀ s. FromSize Number s ⇒ Dimensions → s
dimensionsToSize { width, height } = fromSize (width >< height)

getDimensions ∷ ∀ m. MonadCanvasAction m ⇒ m Dimensions
getDimensions = withCanvas C.getCanvasDimensions

setDimensions ∷ ∀ m. MonadCanvasAction m ⇒ Dimensions → m Unit
setDimensions = withCanvas1 C.setCanvasDimensions

getHeight ∷ ∀ m. MonadCanvasAction m ⇒ m Number
getHeight = withCanvas C.getCanvasHeight

setHeight ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setHeight = withCanvas1 C.setCanvasHeight

getWidth ∷ ∀ m. MonadCanvasAction m ⇒ m Number
getWidth = withCanvas C.getCanvasWidth

setWidth ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
setWidth = withCanvas1 C.setCanvasWidth

-- | Create a data URL for the current canvas contents
toDataUrl ∷ ∀ m. MonadCanvasAction m ⇒ m String
toDataUrl = withCanvas C.canvasToDataURL


getImageData ∷ ∀ m r. MonadCanvasAction m ⇒ ToRegion Number r ⇒ r → m ImageData
getImageData region = withCtx4 C.getImageData x y width height
  where { x, y, width, height } = toRectangle region

-- | Render image data on the canvas. The first argument (`p`) is the point on
-- | the canvas to place the topleft of the data. The second argument (`r`) is
-- | the region of the `ImageData` to render.
putImageDataFull ∷
  ∀ m p r
  . MonadCanvasAction m ⇒ ToPos Number p ⇒ ToRegion Number r
  ⇒ p → r → ImageData → m Unit
putImageDataFull dest dirty img =
  withCtx7 C.putImageDataFull img x y dx dy dw dh
  where (x >< y) = toPos dest
        (Rect (dx >< dy) (dw >< dh)) = toRegion dirty

-- | Render image data on the canvas. The first argument (`p`) is the point on
-- | the canvas to place the topleft of the data.
putImageData ∷
  ∀ m p
  . MonadCanvasAction m ⇒ ToPos Number p
  ⇒ p → ImageData → m Unit
putImageData dest img = withCtx3 C.putImageData img x y
  where (x >< y) = toPos dest

createImageData ∷ ∀ m s. MonadCanvasAction m ⇒ ToSize Number s ⇒ s → m ImageData
createImageData size = withCtx2 C.createImageData w h
  where (w >< h) = toSize size

createImageDataCopy ∷ ∀ m. MonadCanvasAction m ⇒ ImageData → m ImageData
createImageDataCopy = withCtx1 C.createImageDataCopy


imageSource ∷ ∀ m. MonadCanvasAction m ⇒ m CanvasImageSource
imageSource = withCanvas $ pure <<< C.canvasElementToImageSource

drawImage ∷
  ∀ m p. MonadCanvasAction m ⇒ ToPos Number p ⇒ p → CanvasImageSource → m Unit
drawImage pos img = withCtx3 C.drawImage img x y
  where (x >< y) = toPos pos

-- | Draw an image, scaled to fit the provided region
drawImageScale ∷
  ∀ m r
  . MonadCanvasAction m ⇒ ToRegion Number r
  ⇒ r → CanvasImageSource → m Unit
drawImageScale dirty img = withCtx5 C.drawImageScale img x y w h
  where (Rect (x >< y) (w >< h)) = toRegion dirty

-- | Draw an image on the canvas. The first arugment is the region of the image
-- | to draw, and the second argument is the region to draw it in.
drawImageFull ∷
  ∀ m r
  . MonadCanvasAction m ⇒ ToRegion Number r
  ⇒ r → r → CanvasImageSource → m Unit
drawImageFull source dirty img =
  withCtx9 C.drawImageFull img sx sy sw sh dx dy dw dh
  where (Rect (sx >< sy) (sw >< sh)) = toRegion source
        (Rect (dx >< dy) (dw >< dh)) = toRegion dirty

-- | Asynchronously load an image file by specifying its path and a callback
-- | `Effect Unit`.
tryLoadImage' ∷
  ∀ m. MonadEffect m ⇒ String → (Maybe CanvasImageSource → Effect Unit) → m Unit
tryLoadImage' path action = liftEffect $ C.tryLoadImage path action

-- | Asynchronously load an image file by specifying its path and a callback
-- | `CanvasAction`.
tryLoadImage ∷ 
  ∀ m
  . MonadCanvasAction m
  ⇒ String → (Maybe CanvasImageSource → CanvasAction) → m Unit
tryLoadImage path action = do
  ctx ← liftCanvasAction ask
  tryLoadImage' path (runAction ctx <<< action)

-- | Asynchrounously load an image file by specifying its path. The returned
-- | `Aff` will throw an error if the image wasn't found.
loadImageAff ∷ String → Aff CanvasImageSource
loadImageAff path = makeAff \cb → do
  canceled ← Ref.new false
  tryLoadImage' path do
    unlessM (Ref.read canceled) <<< case _ of
      Just image → cb (Right image)
      Nothing → cb (Left do error ("Image \"" <> path <> "\" not found"))
  pure do effectCanceler (Ref.write true canceled)

foreign import setImageSmoothingImpl ∷ Context2D → Boolean → Effect Unit

-- | Set the context's `imageSmoothingEnabled` property
setImageSmoothing ∷ ∀ m. MonadCanvasAction m ⇒ Boolean → m Unit
setImageSmoothing = withCtx1 setImageSmoothingImpl

foreign import getImageSmoothingImpl ∷ Context2D → Effect Boolean

getImageSmoothing ∷ ∀ m. MonadCanvasAction m ⇒ m Boolean
getImageSmoothing = withCtx getImageSmoothingImpl


-- | Create a canvas pattern from an image, which can be used as a fill- or
-- | strokeStyle
createPattern ∷
  ∀ m. MonadCanvasAction m ⇒ CanvasImageSource → PatternRepeat → m CanvasPattern
createPattern = withCtx2 C.createPattern

-- | Constructs a blank linear `CanvasGradient` that can be modified with
-- | `addColorStop`.
createLinearGradient ∷
  ∀ m. MonadCanvasAction m ⇒ LinearGradient → m CanvasGradient
createLinearGradient = withCtx1 C.createLinearGradient

-- | Constructs a blank radial `CanvasGradient` that can be modified with
-- | `addColorStop`.
createRadialGradient ∷
  ∀ m. MonadCanvasAction m ⇒ RadialGradient → m CanvasGradient
createRadialGradient = withCtx1 C.createRadialGradient

-- | Note: Mutates the original `CanvasGradient` and returns `Unit`.
-- | It is recommended to construct gradients with `linearGradient` and
-- | `radialGradient` instead.
addColorStop ∷
  ∀ m r. MonadEffect m ⇒ CanvasColorRep r ⇒ CanvasGradient → Number → r → m Unit
addColorStop grad n col = liftEffect $ C.addColorStop grad n (toColor col)

linearGradient ∷
  ∀ m r f
  . MonadCanvasAction m ⇒ CanvasColorRep r ⇒ Foldable f
  ⇒ LinearGradient → f (Tuple Number r) → m CanvasGradient
linearGradient grad cols = do
  canvasGradient ← createLinearGradient grad
  for_ cols \(Tuple n col) → addColorStop canvasGradient n col
  pure canvasGradient

radialGradient ∷
  ∀ m r f
  . MonadCanvasAction m ⇒ CanvasColorRep r ⇒ Foldable f
  ⇒ RadialGradient → f (Tuple Number r) → m CanvasGradient
radialGradient grad cols = do
  canvasGradient ← createRadialGradient grad
  for_ cols \(Tuple n col) → addColorStop canvasGradient n col
  pure canvasGradient


resetTransform_ ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
resetTransform_ = setTransform_
  { a: 1.0, b: 0.0, c: 0.0, d: 1.0, e: 0.0, f: 0.0 }

setTransform_ ∷ ∀ m. MonadCanvasAction m ⇒ MatrixTransform → m Unit
setTransform_ { a, b, c, d, e, f } = withCtx1 C.setTransform
  { m11: a
  , m12: b
  , m21: c
  , m22: d
  , m31: e
  , m32: f
  }

foreign import getTransformImpl ∷ Context2D → Effect MatrixTransform

getTransform_ ∷ ∀ m. MonadCanvasAction m ⇒ m MatrixTransform
getTransform_ = withCtx getTransformImpl

translate_ ∷ ∀ m. MonadCanvasAction m ⇒ TranslateTransform → m Unit
translate_ = withCtx1 C.translate

scale_ ∷ ∀ m. MonadCanvasAction m ⇒ ScaleTransform → m Unit
scale_ = withCtx1 C.scale

skew_ ∷ ∀ m. MonadCanvasAction m ⇒ SkewTransform → m Unit
skew_ { skewX, skewY } = matrixTransform_
  { a: 1.0, b: skewY, c: skewX, d: 1.0, e: 0.0, f: 0.0 }

rotate_ ∷ ∀ m. MonadCanvasAction m ⇒ Number → m Unit
rotate_ = withCtx1 C.rotate

matrixTransform_ ∷ ∀ m. MonadCanvasAction m ⇒ MatrixTransform → m Unit
matrixTransform_ { a, b, c, d, e, f } = withCtx1 C.transform
  { m11: a
  , m12: b
  , m21: c
  , m22: d
  , m31: e
  , m32: f
  }


beginPath_ ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
beginPath_ = withCtx C.beginPath

stroke_ ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
stroke_ = withCtx C.stroke

fill_ ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
fill_ = withCtx C.fill

clip_ ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
clip_ = withCtx C.clip

lineTo_ ∷ ∀ m p. MonadCanvasAction m ⇒ ToPos Number p ⇒ p → m Unit
lineTo_ pos = withCtx2 C.lineTo x y
  where (x >< y) = toPos pos

moveTo_ ∷ ∀ m p. MonadCanvasAction m ⇒ ToPos Number p ⇒ p → m Unit
moveTo_ pos = withCtx2 C.moveTo x y
  where (x >< y) = toPos pos

closePath_ ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
closePath_ = withCtx C.closePath

arc_ ∷ ∀ m. MonadCanvasAction m ⇒ Arc → m Unit
arc_ = withCtx1 C.arc

rect_ ∷ ∀ m r. MonadCanvasAction m ⇒ ToRegion Number r ⇒ r → m Unit
rect_ = toRectangle >>> withCtx1 C.rect

quadraticCurveTo_ ∷ ∀ m. MonadCanvasAction m ⇒ QuadraticCurve → m Unit
quadraticCurveTo_ = withCtx1 C.quadraticCurveTo

bezierCurveTo_ ∷ ∀ m. MonadCanvasAction m ⇒ BezierCurve → m Unit
bezierCurveTo_ = withCtx1 C.bezierCurveTo

-- | Saves the context, see [`save` on MDN](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/save)
save ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
save = withCtx C.save

-- | Restores the context, see [`restore` on MDN](https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/restore)
restore ∷ ∀ m. MonadCanvasAction m ⇒ m Unit
restore = withCtx C.restore

-- | Runs `save`, then the provided action, then `restore`
restoreAfter ∷ ∀ m a. MonadCanvasAction m ⇒ m a → m a
restoreAfter action = save *> action <* restore
