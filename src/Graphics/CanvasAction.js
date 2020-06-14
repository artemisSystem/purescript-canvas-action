exports.styleIsString = style => typeof style == "string";

exports.styleIsGradient = style => style instanceof CanvasGradient;

exports.styleIsPattern = style => style instanceof CanvasGradient;

exports.getCanvasEffect = ctx => () => ctx.canvas;

exports.setFillStyleImpl = ctx => style => () => void (ctx.fillStyle = style);

exports.setStrokeStyleImpl = ctx => style => () =>
  void (ctx.strokeStyle = style);

exports.getFillStyleImpl = ctx => () => ctx.fillStyle;

exports.getStrokeStyleImpl = ctx => () => ctx.strokeStyle;

exports.setImageSmoothingImpl = ctx => bool => () => void
  (ctx.imageSmoothingEnabled = bool);

exports.getImageSmoothingImpl = ctx => () => ctx.imageSmoothingEnabled;

exports.getTransformImpl = ctx => () => {
  const t = ctx.getTransform();
  return (
    { a: t.a
    , b: t.b
    , c: t.c
    , d: t.d
    , e: t.e
    , f: t.f
    });
}

exports.getTextBaselineImpl = ctx => () => ctx.textBaseline;

exports.setTextBaselineImpl = ctx => baseline => () =>
  void (ctx.textBaseline = baseline);