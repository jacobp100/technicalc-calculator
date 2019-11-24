const SciLine = require("../Value.bs");
const { testUnits } = require("../ScilineTest.bs");
const { convert } = require("../Units/Units.bs");

const c = (value, fromUnits, toUnits) =>
  SciLine.toFloat(convert(value, fromUnits, toUnits));

it("converts simple units", () => {
  expect(
    c(SciLine.one, [[testUnits.meter, 1]], [[testUnits.inch, 1]])
  ).toBeCloseTo(39.37);
  expect(
    c(SciLine.one, [[testUnits.inch, 1]], [[testUnits.meter, 1]])
  ).toBeCloseTo(0.0254);
  expect(
    c(SciLine.ofFloat(2), [[testUnits.meter, 1]], [[testUnits.inch, 1]])
  ).toBeCloseTo(78.74);
});

it("converts exponentiated units", () => {
  expect(
    c(SciLine.one, [[testUnits.meter, 2]], [[testUnits.inch, 2]])
  ).toBeCloseTo(1550);
  expect(
    c(SciLine.one, [[testUnits.inch, 2]], [[testUnits.meter, 2]])
  ).toBeCloseTo(6.452e-4);
});

it("converts between different dimensions", () => {
  expect(
    c(SciLine.one, [[testUnits.acre, 1]], [[testUnits.meter, 2]])
  ).toBeCloseTo(4047);
  expect(
    c(SciLine.one, [[testUnits.liter, 1]], [[testUnits.meter, 3]])
  ).toBeCloseTo(0.001);
});
