const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

imagValues.forEach(v => {
  it(`atanh ${v.title}`, () => {
    expect(SciLine.asinh(v.sciLineValue)).toMatchJsValue(
      mathjs.asinh(v.jsValue)
    );
  });
});
