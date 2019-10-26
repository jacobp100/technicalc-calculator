const BN = require("bn.js");
const rat = require("big-rat");
const Formatting = require("../Formatting/Formatting_Number.bs");

it("should format exponentials without a custom exponent format", () => {
  const format = Formatting.createFormat();
  expect(
    Formatting.formatExponential(undefined, new BN(1), format, rational10))
  ).toEqual(["1", "1"]);
});
