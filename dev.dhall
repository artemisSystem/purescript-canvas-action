let conf = ./spago.dhall

in conf //
  { dependencies = conf.dependencies #
    [ "integers"
    , "psci-support"
    , "web-cssom"
    ]
  , sources = conf.sources # [ "test/**/*.purs" ]
  }
