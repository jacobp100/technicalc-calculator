const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsMatrix } = require("../__test-util__");
const { vector3 } = require("../__test-util__/math-js-vector");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsMatrix });

cartesian([vector3, vector3]).forEach(([a, b]) => {
  it(`${a.title} x ${b.title}`, () => {
    const [i, j, k] = mathjs.cross(a.jsValue, b.jsValue);
    const mathJsValue = mathjs.matrix([[i], [j], [k]]);
    expect(SciLine.mul(a.sciLineValue, b.sciLineValue)).toMatchJsMatrix(
      mathJsValue
    );
  });
});
