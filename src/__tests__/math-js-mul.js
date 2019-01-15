const cartesian = require("cartesian");
const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

cartesian([imagValues, imagValues]).forEach(([a, b]) => {
  it(`${a.title} * ${b.title}`, () => {
    expect(SciLine.mul(a.sciLineValue, b.sciLineValue)).toMatchJsValue(
      mathjs.multiply(a.jsValue, b.jsValue)
    );
  });
});
