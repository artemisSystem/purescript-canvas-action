let main = ./spago.dhall

in main //
  { dependencies = main.dependencies #
    [ "integers"
    , "web-cssom"
    ]
  , sources = main.sources # [ "examples/**/*.purs" ]
  }
