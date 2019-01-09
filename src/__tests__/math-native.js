const cartesian = require("cartesian");
const { Value, toMatchJsValue } = require("../testUtil");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

const range12 = Array.from({ length: 12 }, (_, i) => i + 1);

const range12Values = range12.map(Value.float);

const trigValues = cartesian([range12, range12]).map(
  ([num, denom]) =>
    new Value(
      (Math.PI * num) / denom,
      SciLine.mul(
        SciLine.div(SciLine.of_float(num), SciLine.of_float(denom)),
        SciLine.pi
      ),
      `${num}pi/${denom}`
    )
);

const miscValues = [...range12Values, ...trigValues];

it("works for add", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    const sciLineValue = SciLine.add(lhs.sciLineValue, rhs.sciLineValue);
    const jsValue = lhs.jsValue + rhs.jsValue;
    expect(sciLineValue).toMatchJsValue(jsValue);
  });
});

it("works for sub", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    const sciLineValue = SciLine.sub(lhs.sciLineValue, rhs.sciLineValue);
    const jsValue = lhs.jsValue - rhs.jsValue;
    expect(sciLineValue).toMatchJsValue(jsValue);
  });
});

it("works for mul", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    const sciLineValue = SciLine.mul(lhs.sciLineValue, rhs.sciLineValue);
    const jsValue = lhs.jsValue * rhs.jsValue;
    expect(sciLineValue).toMatchJsValue(jsValue);
  });
});

it("works for div", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    const sciLineValue = SciLine.div(lhs.sciLineValue, rhs.sciLineValue);
    const jsValue = lhs.jsValue / rhs.jsValue;
    expect(sciLineValue).toMatchJsValue(jsValue);
  });
});

it("works for sin", () => {
  trigValues.forEach(v => {
    const sciLineValue = SciLine.sin(v.sciLineValue);
    const jsValue = Math.sin(v.jsValue);
    expect(sciLineValue).toMatchJsValue(jsValue, v.title);
  });
});

it("works for cos", () => {
  trigValues.forEach(v => {
    const sciLineValue = SciLine.cos(v.sciLineValue);
    const jsValue = Math.cos(v.jsValue);
    expect(sciLineValue).toMatchJsValue(jsValue, v.title);
  });
});

it("works for tan", () => {
  trigValues.forEach(v => {
    const sciLineValue = SciLine.tan(v.sciLineValue);
    const jsValue = Math.tan(v.jsValue);
    expect(sciLineValue).toMatchJsValue(jsValue, v.title);
  });
});
