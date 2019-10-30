const { ofInt } = require("../Types.bs");
const { sqrt } = require("../Value.bs");
const { toString } = require("../ScilineTest.bs");

it("Special cases square root of negative numbers", () => {
  expect(toString(sqrt(ofInt(-2)))).toBe("sqrt(2)i");
});
