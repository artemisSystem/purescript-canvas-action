# purescript-canvas-action

## Installation

```sh
spago install canvas-action
```

## Documentation

This package introduces a monad and type class for working with the HTML5 canvas
without having to pass a `Context2D` argument to every function that needs it.
Assumes familiarity with the canvas API.

### Reference

Module reference is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-canvas-action).

## Developing

To test one of the examples, run `npm run example:<Name>`, replacing `<Name>`
with the name of the example or test you want to run. This will create a
server that auto-refreshes every time the built javascript changes, and open a
browser window. It does not build the purescript for you except once at the
start, so if you don't have an editor that rebuilds it automatically, you'll
want to run `npm run watch:examples` at the same time. The examples are located
in `/examples/Example/<Name>`.

Open the REPL using `npm run repl` (`npx spago -x dev.dhall repl`).

You can view all the scripts (including all the examples) with `npm run`.
