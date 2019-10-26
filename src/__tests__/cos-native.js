const { toMatchJsValue } = require("../__test-util__");
const { trigValues } = require("../__test-util__/math-native");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test.each(trigValues)("cos(%s)", v => {
  expect(SciLine.cos(v.sciLineValue)).toMatchJsValue(Math.cos(v.jsValue));
});
