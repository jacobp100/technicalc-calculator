const { ofFloat, ofInt } = require("../Types.bs");
const { sqrt } = require("../Value.bs");
const { toString } = require("../ScilineTest.bs");

it("Special cases square root of negative numbers", () => {
  expect(toString(sqrt(ofInt(-2)))).toBe("sqrt(2)i");
});

it("Handles large numbers", () => {
  expect(toString(sqrt(ofFloat(1e6)))).toBe("1,000");
  expect(toString(sqrt(ofFloat(1e12)))).toBe("1,000,000");
});
