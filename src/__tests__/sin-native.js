const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(trigValues)("sin(%s)", v => {
  expect(SciLine.sin(v.sciLineValue)).toMatchJsValue(Math.sin(v.jsValue));
});
