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
    { m11: t.m11
    , m12: t.m12
    , m21: t.m21
    , m22: t.m22
    , m31: t.m31
    , m32: t.m32
    });
}

exports.getTextBaselineImpl = ctx => () => ctx.textBaseline;

exports.setTextBaselineImpl = ctx => baseline => () =>
  void (ctx.textBaseline = baseline);