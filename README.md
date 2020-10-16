# purescript-canvas-action

## Installation

```sh
spago install canvas-action
```

## Developing

Make sure you have the purescript compiler, parcel and spago installed and
available on your PATH.

To test one of the examples, run `npm run example-<Name>`, replacing `<Name>`
with the name of the example or test you want to run. This will create a
server that auto-rebuilds every time you make a change to its source, and open a
browser window. The examples are located in `/test/Test/<Name>`.

Open the REPL using `npm run repl` (`spago -x dev.dhall repl`).

## Documentation

This is a package that introduces a monad for working with the HTML5 canvas
without having to pass a `Context2D` argument to every function that needs it.
It also has free monads for representing canvas transformations and paths in a
pure manner. These can later be run in a `MonadCanvasAction`. More info in the
respective modules.

### Reference

Module reference is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-canvas-action).
