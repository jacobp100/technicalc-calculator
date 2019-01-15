const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

imagValues.forEach(v => {
  it(v.title, () => {
    expect(SciLine.log(v.sciLineValue)).toMatchJsValue(mathjs.log(v.jsValue));
  });
});
