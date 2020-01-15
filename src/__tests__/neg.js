const { toMatchJsValue } = require("../__test-util__");
const SciLine = require("../Value.bs");

expect.extend({ toMatchJsValue });

test("-1", () => {
  expect(SciLine.neg(SciLine.one)).toMatchJsValue(-1);
});

test("-pi (float value)", () => {
  expect(SciLine.neg(SciLine.ofFloat(Math.PI))).toMatchJsValue(-Math.PI);
});
