const NumberFormat = require("../NumberFormat.bs");

it("should format exponentials without a custom exponent format", () => {
  const format = NumberFormat.create_format();
  expect(NumberFormat.format_exponential(1, undefined, format, 10)).toBe("1e1");
});

it("should format exponentials with custom exponent format", () => {
  const format = NumberFormat.create_format();
  expect(NumberFormat.format_exponential(1, "\\times10^{$}", format, 10)).toBe(
    "1\\times10^{1}"
  );
});
