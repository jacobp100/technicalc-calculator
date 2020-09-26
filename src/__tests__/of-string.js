const TechniCalcTest = require("../TechniCalcTest.bs");

it("parses strings", () => {
  expect(TechniCalcTest.toString(TechniCalcTest.valueOfString("100"))).toBe(
    "100"
  );
});

it("parses strings in other bases", () => {
  expect(
    TechniCalcTest.toString(TechniCalcTest.valueOfStringBase(16, "100"))
  ).toBe("256");
});
