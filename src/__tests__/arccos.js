const mathjs = require("mathjs");
const { toMatchJsValue } = require("../__test-util__");
const { imagValues } = require("../__test-util__/math-js");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

imagValues.forEach(v => {
  it(`acos ${v.title}`, () => {
    expect(SciLine.acos(v.sciLineValue)).toMatchJsValue(
      mathjs.acos(v.jsValue)
    );
  });
});
