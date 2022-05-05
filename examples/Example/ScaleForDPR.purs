module Example.ScaleForDPR
  ( getDPRChange
  , scaleForDPROnce
  , scaleForDPR
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Vector.Polymorphic (Vector2, (><))
import Data.Vector.Polymorphic.Class (fromSize)
import Effect.Aff (Aff, effectCanceler, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Graphics.CanvasAction (class MonadCanvasAction, class MonadCanvasAff, getCanvas, setDimensions)
import Graphics.CanvasAction.CSSOM (devicePixelRatio, matchMedia, toEventTarget)
import Graphics.CanvasAction.Transformation (scale, setTransform)
import Web.CSSOM.CSSStyleDeclaration (setProperty) as CSS
import Web.CSSOM.ElementCSSInlineStyle (fromHTMLElement, style) as CSS
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML.HTMLCanvasElement as CanvasElem

-- | Resolves with the new devicePixelRatio next time it changes
getDPRChange ∷ Aff Number
getDPRChange = makeAff \cb → do
  listenerRef ← Ref.new Nothing
  currentDPR ← devicePixelRatio
  mediaQueryList ← toEventTarget <$> matchMedia do
    "(resolution: " <> show currentDPR <> "dppx)"
  let
    removeListener = Ref.read listenerRef >>= traverse_ \listener →
      removeEventListener (EventType "change") listener false mediaQueryList
  listener ← eventListener \_ → do
    newDPR ← devicePixelRatio
    removeListener
    cb (Right newDPR)
  addEventListener (EventType "change") listener false mediaQueryList
  Ref.write (Just listener) listenerRef
  pure (effectCanceler removeListener)

-- | Note: This sets the canvas dimensions, which clears all canvas data. To
-- | keep a consistent canvas size, set it using CSS.
scaleForDPROnce ∷ ∀ m. MonadCanvasAction m ⇒ Vector2 Number → Number → m Unit
scaleForDPROnce size dpr = do
  setDimensions (fromSize (pure dpr * size))
  setTransform (scale dpr dpr)

iterate ∷ ∀ m a void. Bind m ⇒ a → (a → m a) → m void
iterate a m = m a >>= (_ `iterate` m)

setCanvasCSSSize ∷ ∀ m. MonadCanvasAction m ⇒ Vector2 Number → m Unit
setCanvasCSSSize (w >< h) = do
  canvas ← getCanvas
  liftEffect do
    canvasStyle ←
      (CSS.style <<< CSS.fromHTMLElement) (CanvasElem.toHTMLElement canvas)
    CSS.setProperty "width" (show w <> "px") canvasStyle
    CSS.setProperty "height" (show h <> "px") canvasStyle

-- | Scales canvas when devicePixelRatio changes, and runs a `MonadCanvasAff`
-- | after. Infinite loop. Sets CSS size once first. Also see `scaleForDPROnce`.
scaleForDPR ∷ ∀ m void. MonadCanvasAff m ⇒ Vector2 Number → m Unit → m void
scaleForDPR size action = do
  setCanvasCSSSize size
  dpr0 ← devicePixelRatio
  iterate dpr0 \dpr → do
    scaleForDPROnce size dpr
    action
    liftAff getDPRChange
