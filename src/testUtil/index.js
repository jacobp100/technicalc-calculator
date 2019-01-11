const cartesian = require("cartesian");
const SciLine = require("../SciLine.bs");

module.exports.Value = class Value {
  constructor(jsValue, sciLineValue, title = String(jsValue)) {
    this.title = title;
    this.sciLineValue = sciLineValue;
    this.jsValue = jsValue;
  }

  static float(n, title = String(n)) {
    return new Value(n, SciLine.of_float(n), title);
  }

  static pi(n = 1, title = `${n}pi`) {
    return new Value(n * Math.PI, SciLine.pi, title);
  }

  toString() {
    return `(js: ${this.jsValue}, sciline: ${SciLine.to_string(
      this.sciLineValue
    )})`;
  }
};

const range12 = Array.from({ length: 12 }, (_, i) => i + 1);
const existingFractions = new Set();
const fractionsTo12 = cartesian([[0, ...range12], range12]).filter(([a, b]) => {
  const key = (a / b).toFixed(8);
  if (!existingFractions.has(key)) {
    existingFractions.add(key);
    return true;
  }
  return false;
});

module.exports.range12 = range12;
module.exports.fractionsTo12 = fractionsTo12;

const isCloseTo = (a, b) => {
  if (!Number.isFinite(a)) return !Number.isFinite(b);

  const aMagnitude = Math.floor(Math.log10(Math.abs(a)));
  const bMagnitude = Math.floor(Math.log10(Math.abs(b)));

  const normalizerMagnitude =
    Math.abs(aMagnitude - bMagnitude) <= 1
      ? Math.max(1, Math.min(aMagnitude - 2, bMagnitude - 2))
      : 1;
  const normalizer = 10 ** normalizerMagnitude;

  return Math.abs(a - b) / normalizer < 1e-8;
};

const asComplex = a => {
  const comp = typeof a === "number" ? [a, 0] : [a.re, a.im];
  return !Number.isFinite(comp[0]) || !Number.isFinite(comp[1])
    ? [NaN, NaN]
    : comp;
};

module.exports.toMatchJsValue = (received, expected) => {
  const [actualRe, actualIm] = SciLine.to_floats(received);
  const [expectedRe, expectedIm] = asComplex(expected);

  const pass =
    isCloseTo(actualRe, expectedRe) && isCloseTo(actualIm, expectedIm);

  return {
    message: () =>
      `expected ${SciLine.to_string(received)} ${
        pass ? "not " : ""
      }to be close to ${expectedRe}+${expectedIm}i`,
    pass
  };
};
