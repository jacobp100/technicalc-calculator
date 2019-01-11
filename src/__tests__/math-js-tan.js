const mathjs = require("mathjs");
const { toMatchJsValue } = require("../testUtil");
const { trigValues } = require("../testUtil/math-js");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

trigValues.forEach(v => {
  it(v.title, () => {
    expect(SciLine.tan(v.sciLineValue)).toMatchJsValue(mathjs.tan(v.jsValue));
  });
});
