const cartesian = require("cartesian");
const { range } = require("lodash");
const mathjs = require("mathjs");
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

  static complex(re, im, title = `${re}+${im}i`) {
    return new Value(
      mathjs.complex(re, im),
      SciLine.of_complex_floats(re, im),
      title
    );
  }

  static e(n = 1, title = `${n}e`) {
    return new Value(
      n * Math.E,
      SciLine.mul(SciLine.of_float(n), SciLine.e),
      title
    );
  }

  static pi(n = 1, title = `${n}pi`) {
    return new Value(
      n * Math.PI,
      SciLine.mul(SciLine.of_float(n), SciLine.pi),
      title
    );
  }

  toString() {
    return `(js: ${this.jsValue}, sciline: ${SciLine.to_string(
      this.sciLineValue
    )})`;
  }
};

const range12 = range(1, 12 + 1);
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
  const resolved = SciLine.resolve(received);

  const [actualRe, actualIm] = SciLine.to_complex_floats(resolved);
  const [expectedRe, expectedIm] = asComplex(expected);

  const pass =
    isCloseTo(actualRe, expectedRe) && isCloseTo(actualIm, expectedIm);

  return {
    message: () =>
      `expected ${SciLine.to_string(resolved)} ${
        pass ? "not " : ""
      }to be close to ${expectedRe}+${expectedIm}i`,
    pass
  };
};

module.exports.toMatchJsMatrix = (received, expected) => {
  const resolved = SciLine.resolve(received);

  const sciLineElements = SciLine.to_complex_floats_matrix(resolved);

  let allPass = true;
  expected.forEach((mathJsElement, [row, column]) => {
    const [actualRe, actualIm] = sciLineElements[row][column];
    const [expectedRe, expectedIm] = asComplex(mathJsElement);
    const pass =
      isCloseTo(actualRe, expectedRe) && isCloseTo(actualIm, expectedIm);
    allPass = allPass && pass;
  });

  return {
    message: () =>
      `expected ${SciLine.to_string(resolved)} ${
        allPass ? "not " : ""
      }to be close to ${expected}`,
    pass: allPass
  };
};
