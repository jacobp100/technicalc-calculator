const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

imagValues.forEach(v => {
  it(`asin ${v.title}`, () => {
    expect(SciLine.asin(v.sciLineValue)).toMatchJsValue(
      mathjs.asin(v.jsValue)
    );
  });
});
