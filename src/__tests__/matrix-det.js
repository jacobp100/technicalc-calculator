const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { matrix2x2, matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

matrix2x2.forEach(v => {
  it(`abs ${v.title}`, () => {
    const mathJsValue = mathjs.det(v.jsValue);
    expect(SciLine.abs(v.sciLineValue)).toMatchJsValue(mathJsValue);
  });
});

matrix3x3.forEach(v => {
  it(`abs ${v.title}`, () => {
    const mathJsValue = mathjs.det(v.jsValue);
    expect(SciLine.abs(v.sciLineValue)).toMatchJsValue(mathJsValue);
  });
});

describe("Type checking", () => {
  it("Cannot add incompatible sizes", () => {
    const value = SciLine.resolve(
      SciLine.abs(
        SciLine.matrix_of_elements(
          3,
          2,
          [3, 7, 8, 8, 6, 6].map(SciLine.of_float)
        )
      )
    );
    expect(SciLine.to_string(value)).toBe("NaN");
  });
});
