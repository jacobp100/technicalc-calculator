const SciLine = require("../SciLine.bs");

const stringOf = s => SciLine.to_string(SciLine.resolve(s));

it("Does not simplify 2sqrt(2)", () => {
  expect(
    stringOf(
      SciLine.mul(SciLine.of_float(2), SciLine.sqrt(SciLine.of_float(2)))
    )
  ).toEqual("2sqrt(2)");
});

it("Simplifies sqrt(4)", () => {
  expect(stringOf(SciLine.sqrt(SciLine.of_float(4)))).toEqual("2");
});

it("Simplifies sqrt(8)", () => {
  expect(stringOf(SciLine.sqrt(SciLine.of_float(8)))).toEqual("2sqrt(2)");
});

it("Does not simplify sqrt(6)", () => {
  expect(stringOf(SciLine.sqrt(SciLine.of_float(6)))).toEqual("sqrt(6)");
});

it("Simplify sqrt(12)", () => {
  expect(stringOf(SciLine.sqrt(SciLine.of_float(12)))).toEqual("2sqrt(3)");
});

it("Simplifies exp(0)", () => {
  expect(stringOf(SciLine.exp(SciLine.of_float(0)))).toEqual("1");
});

it("Does not simplify exp(>0)", () => {
  expect(stringOf(SciLine.exp(SciLine.of_float(1)))).toEqual("exp(1)");
  expect(stringOf(SciLine.exp(SciLine.of_float(2)))).toEqual("exp(2)");
  expect(stringOf(SciLine.exp(SciLine.of_float(3)))).toEqual("exp(3)");
  expect(stringOf(SciLine.exp(SciLine.of_float(-1)))).toEqual("exp(-1)");
});

it("Does not simplify pi", () => {
  expect(stringOf(SciLine.pi)).toEqual("pi");
});
