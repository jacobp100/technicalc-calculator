const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-js");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

trigValues.forEach(v => {
  it(`tan ${v.title}`, () => {
    expect(SciLine.tan(v.sciLineValue)).toMatchJsValue(mathjs.tan(v.jsValue));
  });
});
