const { toMatchJsValue } = require("../__test-util__");
const TechniCalc = require("../Value.bs");
const Types = require("../Types.bs");

expect.extend({ toMatchJsValue });

test("Numerical results", () => {
  expect(
    TechniCalc.add(
      TechniCalc.ofFloat(100),
      Types.percent(TechniCalc.ofFloat(20))
    )
  ).toMatchJsValue(120);
  expect(
    TechniCalc.sub(
      TechniCalc.ofFloat(100),
      Types.percent(TechniCalc.ofFloat(20))
    )
  ).toMatchJsValue(80);

  expect(
    TechniCalc.mul(
      TechniCalc.ofFloat(100),
      Types.percent(TechniCalc.ofFloat(20))
    )
  ).toMatchJsValue(20);
  expect(
    TechniCalc.mul(
      Types.percent(TechniCalc.ofFloat(20)),
      TechniCalc.ofFloat(100)
    )
  ).toMatchJsValue(20);

  expect(
    TechniCalc.div(
      TechniCalc.ofFloat(100),
      Types.percent(TechniCalc.ofFloat(100))
    )
  ).toMatchJsValue(50);
  expect(
    TechniCalc.div(
      TechniCalc.ofFloat(100),
      Types.percent(TechniCalc.ofFloat(-20))
    )
  ).toMatchJsValue(125);
});
