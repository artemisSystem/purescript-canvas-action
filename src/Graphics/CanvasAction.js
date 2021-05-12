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
