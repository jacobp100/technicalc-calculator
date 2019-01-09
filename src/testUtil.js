const SciLine = require("./SciLine.bs");

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
    return `(js: ${this.jsValue}, sciline: ${SciLine.string_of_value(
      this.sciLineValue
    )})`;
  }
};

module.exports.toMatchJsValue = (
  received,
  expected,
  title = "No title given"
) => {
  const [actualRe, actualIm] = SciLine.complex_tuple(received);
  const expectedRe = typeof expected === "number" ? expected : expected.re;
  const expectedIm = typeof expected === "number" ? 0 : expected.im;
  const diffRe = Math.abs(actualRe - expectedRe);
  const diffIm = Math.abs(actualIm - expectedIm);

  const pass = diffRe < 1e8 && diffIm < 1e8;

  return {
    message: () =>
      `expected ${SciLine.string_of_float(received)} ${
        pass ? "not" : ""
      } to be close to ${expectedRe}+${expectedIm}i (${title})`,
    pass
  };
};
