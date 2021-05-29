exports.devicePixelRatioImpl = () => window.devicePixelRatio;

exports.matchMedia = mediaQueryString => () =>
  window.matchMedia(mediaQueryString);
