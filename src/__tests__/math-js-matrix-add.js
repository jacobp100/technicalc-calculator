const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { matrix2x2 } = require("../testUtil/math-js-matrix");

cartesian([matrix2x2, matrix2x2]).forEach((a, b) => {
  it(`${a.title} + ${b.title}`, () => {
    const mathJsValue = mathjs.add(a.jsValue, b.jsValue);
    expect(SciLine.add(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
      mathJsValue
    );
  });
});
