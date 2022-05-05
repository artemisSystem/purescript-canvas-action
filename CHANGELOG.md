# Changelog

All notable changes to this project will (hopefully) be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [v9.0.0] - 2022-05-05

### Breaking Changes

- Update to PureScript 0.15

## [v8.0.0] - 2022-02-16

### Breaking Changes

- Made `CanvasColor` a subclass of `CanvasStyle`.

### Additions

- Added `toStyleRepDefault`.

## [v7.0.0] - 2021-05-29

### Breaking Changes

- Removed `type CanvasAction = CanvasActionM Unit` type alias, renamed
`CanvasActionM` to `CanvasAction`.

- Removed all transformation-related functions from `Graphics.CanvasAction`.
- Removed all path-related functions from `Graphics.CanvasAction`.
- Unexported all `withCtx*` and `withCanvas*` functions.
- Renamed the `CanvasStyleRep` class to `CanvasStyle` and the `CanvasStyle` type
to `CanvasStyleRep`.

- Renamed the `CanvasColorRep` class to `CanvasColor` and the `CanvasColor` type
to `CanvasColorRep`.

- Started using `HTMLCanvasElement` from `web-html` instead of `CanvasElement`
from `canvas`.

- The type signature of `withMidPos` changed to take a function of type
`(Vector2 Number → m a)` instead of `(∀ p. ToPos Number p ⇒ p → m a)`.

- Removed `dimensionsToSize` (obsoleted by `polymorphic-vectors` v3.0.0).
- Made changes in `Graphics.CanvasAction.Run` to fit with changes in `run`
v4.0.0.

- Stopped using the `canvas ∷ CanvasAction` field and started using
`ctx ∷ Reader Context2D` and `effect ∷ Effect`, adding `aff ∷ Aff` for
`CanvasAff`.

- Changed `Graphics.CanvasAction.Path` to use a `PathAction` monad instead of the
`PathM` free monad. Construct paths from the primitives, get a `Path2D` using
`runPath`, and draw that to the canvas using `fill` or `stroke`, or create a
clipping path with `clip`.

- Changed `Graphics.CanvasAction.Transformation` to use a `DOMMatrix` monoid
which can be built up from primitives, and have its transformations applied
using for example `transformedBy`, `transformPoint`, or `setTransform`.

### Additions

- Added `CanvasAff`.
- Added `MonadCanvasAff` typeclass.
- Added `Graphics.CanvasAction.CSSOM`, featuring `devicePixelRatio` and CSS
media queries.
