const { toMatchJsValue } = require("../__test-util__");
const SciLine = require("../Value.bs");
const Types = require("../Types.bs");

expect.extend({ toMatchJsValue });

test("Numerical results", () => {
  expect(
    SciLine.add(SciLine.ofFloat(100), Types.percent(SciLine.ofFloat(20)))
  ).toMatchJsValue(120);
  expect(
    SciLine.sub(SciLine.ofFloat(100), Types.percent(SciLine.ofFloat(20)))
  ).toMatchJsValue(80);

  expect(
    SciLine.mul(SciLine.ofFloat(100), Types.percent(SciLine.ofFloat(20)))
  ).toMatchJsValue(20);
  expect(
    SciLine.mul(Types.percent(SciLine.ofFloat(20)), SciLine.ofFloat(100))
  ).toMatchJsValue(20);

  expect(
    SciLine.div(SciLine.ofFloat(100), Types.percent(SciLine.ofFloat(100)))
  ).toMatchJsValue(50);
  expect(
    SciLine.div(SciLine.ofFloat(100), Types.percent(SciLine.ofFloat(-20)))
  ).toMatchJsValue(125);
});
