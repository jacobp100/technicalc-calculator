const BN = require("bn.js");
const rat = require("big-rat");
const NumberFormat = require("../NumberFormat.bs");

it("should format exponentials without a custom exponent format", () => {
  const format = NumberFormat.create_format();
  expect(
    NumberFormat.format_exponential(new BN(1), undefined, format, rat(10))
  ).toBe("1e1");
});

it("should format exponentials with custom exponent format", () => {
  const format = NumberFormat.create_format();
  expect(
    NumberFormat.format_exponential(new BN(1), "\\times10^{$}", format, rat(10))
  ).toBe("1\\times10^{1}");
});
