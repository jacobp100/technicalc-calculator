const cartesian = require("cartesian");
const { range } = require("lodash");
const mathjs = require("mathjs");
const SciLine = require("../Value.bs");
const SciLineTest = require("../SciLineTest.bs");

module.exports.Value = class Value {
  constructor(jsValue, sciLineValue, title = String(jsValue)) {
    this.title = title;
    this.sciLineValue = sciLineValue;
    this.jsValue = jsValue;
  }

  static float(n, title = String(n)) {
    return new Value(n, SciLine.ofFloat(n), title);
  }

  static complex(re, im, title = `${re}+${im}i`) {
    return new Value(
      mathjs.complex(re, im),
      SciLineTest.ofComplexFloats(re, im),
      title
    );
  }

  static e(n = 1, title = `${n}e`) {
    return new Value(
      n * Math.E,
      SciLine.mul(SciLine.ofFloat(n), SciLine.e),
      title
    );
  }

  static pi(n = 1, title = `${n}pi`) {
    return new Value(
      n * Math.PI,
      SciLine.mul(SciLine.ofFloat(n), SciLine.pi),
      title
    );
  }

  toString() {
    return `(js: ${this.jsValue}, sciline: ${SciLineTest.toString(
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
  const [actualRe, actualIm] = SciLineTest.toComplexFloats(received);
  const [expectedRe, expectedIm] = asComplex(expected);

  const pass =
    isCloseTo(actualRe, expectedRe) && isCloseTo(actualIm, expectedIm);

  return {
    message: () =>
      `expected ${SciLineTest.toString(received)} ${
        pass ? "not " : ""
      }to be close to ${expectedRe}+${expectedIm}i`,
    pass
  };
};

module.exports.toMatchJsMatrix = (received, expected) => {
  const sciLineElements = SciLineTest.toComplexFloatsMatrix(received);

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
      `expected ${SciLineTest.toString(received)} ${
        allPass ? "not " : ""
      }to be close to ${expected}`,
    pass: allPass
  };
};
