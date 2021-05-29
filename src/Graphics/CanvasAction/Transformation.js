exports.multiplyDOMMatrix = m1 => m2 => m1.multiply(m2);

exports.invertDOMMatrix = m => m.inverse();

exports.fromRecord = ({ a, b, c, d, e, f }) =>
  new DOMMatrixReadOnly([a, b, c, d, e, f]);

exports.toRecord = m => ({ a: m.a, b: m.b, c: m.c, d: m.d, e: m.e, f: m.f });

exports.getTransformImpl = ctx => () => ctx.getTransform();
