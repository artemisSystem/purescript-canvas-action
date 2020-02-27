-- | This module defines a free monad for working with canvas paths in a more
-- | pleasant way. Make your paths by combining `lineTo`, `moveTo`, `arc`,
-- | `rect`, `quadraticCurveTo`, `bezierCurveTo`, and `closePath` using `<>` or
-- | `bind` (`do` notation), and use them with `fillPath`, `fillPathWith`,
-- | `strokePath`, `strokePathWith`, and `clip`.
-- |
-- | Example:
-- | ```purescript
-- | action :: CanvasAction
-- | action = fillPathWith "red" path
-- |
-- | path :: Path
-- | path = do
-- |   moveTo (100.0 >< 100.0)
-- |   lineTo (200.0 >< 10.0)
-- |   lineTo (10.0 >< 100.0)
-- |   closePath
-- |   polygon
-- |     [ 0.0 >< 0.0
-- |     , 20.0 >< 200.0
-- |     , 100.0 >< 175.0
-- |     , 30.0 >< 150.0
-- |     ]
-- |   circle (100.0 >< 100.0) 50.0
-- | ```
-- | `action` is a `CanvasAction` that can be run in `Effect` with `runAction`
-- | from `Graphics.CanvasAction`.

module Graphics.CanvasAction.Path
  ( PathF
  , PathM
  , Path

  , lineTo
  , moveTo
  , arc
  , rect
  , quadraticCurveTo
  , bezierCurveTo
  , closePath

  , fillPath
  , fillPathWith
  , strokePath
  , strokePathWith
  , clip

  , polygon
  , circle
  ) where

import Prelude

import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Foldable (class Foldable, traverse_)
import Data.List (List(..), fromFoldable, (:))
import Graphics.Canvas (Arc, QuadraticCurve, BezierCurve)
import Graphics.CanvasAction (class CanvasStyleRep, class MonadCanvasAction)
import Graphics.CanvasAction (arc, beginPath, bezierCurveTo, clip, closePath, fill, lineTo, moveTo, quadraticCurveTo, rect, stroke, filled, stroked) as CA
import Data.Vector.Polymorphic (Rect, Vector2, (><))
import Data.Vector.Polymorphic.Class (class ToPos, class ToRegion, toPos, toRegion)
import Math (tau)


data PathF a
  = LineTo           (Vector2 Number) a
  | MoveTo           (Vector2 Number) a
  | PathArc          Arc              a
  | PathRect         (Rect Number)    a
  | QuadraticCurveTo QuadraticCurve   a
  | BezierCurveTo    BezierCurve      a
  | ClosePath                         a

derive instance functorPathF :: Functor PathF

type PathM = Free PathF
type Path = PathM Unit

lineTo :: forall p. ToPos Number p => p -> Path
lineTo pos = liftF $ LineTo pos' unit
  where pos' = toPos pos

moveTo :: forall p. ToPos Number p => p -> Path
moveTo pos = liftF $ MoveTo pos' unit
  where pos' = toPos pos

arc :: Arc -> Path
arc a = liftF $ PathArc a unit

rect :: forall r. ToRegion Number r => r -> Path
rect region = liftF $ PathRect region' unit
  where region' = toRegion region

quadraticCurveTo :: QuadraticCurve -> Path
quadraticCurveTo q = liftF $ QuadraticCurveTo q unit

bezierCurveTo :: BezierCurve -> Path
bezierCurveTo q = liftF $ BezierCurveTo q unit

closePath :: Path
closePath = liftF $ ClosePath unit


interpret :: forall m. MonadCanvasAction m => MonadRec m => PathM ~> m
interpret = runFreeM go
  where
    go (LineTo           p a) = CA.lineTo           p $> a
    go (MoveTo           p a) = CA.moveTo           p $> a
    go (PathArc          r a) = CA.arc              r $> a
    go (PathRect         r a) = CA.rect             r $> a
    go (QuadraticCurveTo q a) = CA.quadraticCurveTo q $> a
    go (BezierCurveTo    b a) = CA.bezierCurveTo    b $> a
    go (ClosePath          a) = CA.closePath          $> a

fillPath :: forall m. MonadCanvasAction m => MonadRec m => PathM ~> m
fillPath path = CA.beginPath *> interpret path <* CA.fill

fillPathWith
  :: forall m r
   . MonadCanvasAction m => MonadRec m => CanvasStyleRep r
  => r -> PathM ~> m
fillPathWith color = CA.filled color <<< fillPath

strokePath :: forall m. MonadCanvasAction m => MonadRec m => PathM ~> m
strokePath path = CA.beginPath *> interpret path <* CA.stroke

strokePathWith
  :: forall m r
   . MonadCanvasAction m => MonadRec m => CanvasStyleRep r
  => r -> PathM ~> m
strokePathWith color = CA.stroked color <<< strokePath

clip :: forall m. MonadCanvasAction m => MonadRec m => PathM ~> m
clip path = CA.beginPath *> interpret path <* CA.clip

-- | Draw a polygon with the specified points. Starts and ends on the first
-- | point.
polygon :: forall p f. ToPos Number p => Foldable f => f p -> Path
polygon = go <<< fromFoldable
  where
    go (p0 : ps) = moveTo p0 *> traverse_ lineTo ps *> closePath
    go Nil = mempty

-- | Draw a circle with the specified center and radius. Ends on the rightmost
-- | point of the circle.
circle :: forall p. ToPos Number p => p -> Number -> Path
circle pos radius = do
  moveTo (x + radius >< y)
  arc { x, y, radius, start: 0.0, end: tau }
    where (x >< y) = toPos pos