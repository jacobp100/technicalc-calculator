const { range } = require("lodash");
const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { Value, toMatchJsMatrix } = require("../__test-util__");
const { matrix3x3 } = require("../__test-util__/math-js-matrix");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsMatrix });

describe("3x3", () => {
  cartesian([matrix3x3, range(0, 2 + 1).map(Value.float)]).forEach(([a, b]) => {
    it(`${a.title} ** ${b.title}`, () => {
      const mathJsValue = mathjs.pow(a.jsValue, b.jsValue);
      expect(SciLine.pow(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
        mathJsValue
      );
    });
  });
});

describe("Pow -1", () => {
  // Checked on Wolfram Alpha
  it("2x2 ** -1", () => {
    const out = SciLine.toString(
      SciLine.resolve(
        SciLine.pow(
          SciLine.matrixOfElements(2, 2, [3, 7, 8, 9].map(SciLine.ofFloat)),
          SciLine.ofFloat(-1)
        )
      )
    );
    expect(out).toBe("{{-9/29, 7/29}, {8/29, -3/29}}");
  });

  it("3x3 ** -1", () => {
    const out = SciLine.toString(
      SciLine.resolve(
        SciLine.pow(
          SciLine.matrixOfElements(
            3,
            3,
            [3, 7, 8, 9, 1, 3, 9, 5, 8].map(SciLine.ofFloat)
          ),
          SciLine.ofFloat(-1)
        )
      )
    );
    expect(out).toBe(
      "{{7/48, 1/3, -13/48}, {15/16, 1, -21/16}, {-3/4, -1, 5/4}}"
    );
  });
});
