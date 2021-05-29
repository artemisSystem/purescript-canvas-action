exports.addPathImpl = path2 => matrix => path1 => () =>
  void path1.addPath(path2, matrix);

exports.closePathImpl = path => () => void path.closePath();

exports.moveToImpl = x => y => path => () => void path.moveTo(x, y);

exports.lineToImpl = x => y => path => () => void path.lineTo(x, y);

exports.bezierCurveToImpl = cp1x => cp1y => cp2x => cp2y => x => y =>
  path => () => void path.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y);

exports.quadraticCurveToImpl = cpx => cpy => x => y =>
  path => () => void path.quadraticCurveTo(cpx, cpy, x, y);

exports.arcToImpl = x1 => y1 => x2 => y2 => radius => path => () =>
  void path.arcTo(x1, y1, x2, y2, radius);

exports.ellipseImpl = x => y => radiusX => radiusY => rotation => startAngle =>
  endAngle => anticlockwise => path => () => void path.ellipse(
    x, y,
    radiusX, radiusY,
    rotation,
    startAngle, endAngle,
    anticlockwise
  );

exports.rectImpl = x => y => width => height => path => () =>
  void path.rect(x, y, width, height)


exports.newPath2DImpl = () => new Path2D();

exports.fillImpl = ctx => path => fillRule => () =>
  void ctx.fill(path, fillRule);

exports.strokeImpl = ctx => path => () => void ctx.stroke(path);

exports.clipImpl = ctx => path => fillRule => () =>
  void ctx.clip(path, fillRule);
