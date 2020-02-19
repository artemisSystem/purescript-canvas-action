{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "canvas-action"
, dependencies =
  [ "canvas"
  , "colors"
  , "console"
  , "effect"
  , "exceptions"
  , "foldable-traversable"
  , "free"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "partial"
  , "polymorphic-vectors"
  , "prelude"
  , "psci-support"
  , "transformers"
  , "tuples"
  , "web-html"
  ]
, license = "MIT"
, repository = "https://github.com/3ddyy/purescript-canvas-action.git"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
