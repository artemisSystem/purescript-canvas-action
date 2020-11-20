{ name = "canvas-action"
, dependencies =
    [ "aff"
    , "canvas"
    , "colors"
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
    , "run"
    , "transformers"
    , "tuples"
    , "typelevel-prelude"
    , "web-html"
    ]
, license = "MIT"
, repository = "https://github.com/artemisSystem/purescript-canvas-action.git"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
