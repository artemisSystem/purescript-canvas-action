let main = ./spago.dhall

in main //
  { dependencies = main.dependencies #
    [ "integers"
    , "psci-support"
    , "web-cssom"
    ]
  , sources = main.sources # [ "examples/**/*.purs" ]
  }
