const cartesian = require("cartesian");
const { Value, toMatchJsValue } = require("../testUtil");
const SciLine = require("../SciLine.bs");

expect.extend({ toMatchJsValue });

const sciLineString = x => SciLine.to_string(x.sciLineValue);

const range12 = Array.from({ length: 12 }, (_, i) => i + 1);

const range12NegativeValues = range12.map(x => Value.float(-x));
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

const trigNegativeValues = cartesian([
  range12NegativeValues,
  range12NegativeValues
]).map(
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

const miscValues = [...range12Values, ...range12NegativeValues, ...trigValues];

it("works for add", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    expect(SciLine.add(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue + rhs.jsValue,
      `${sciLineString(lhs)} + ${sciLineString(rhs)}`
    );
  });
});

it("works for sub", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    expect(SciLine.sub(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue - rhs.jsValue,
      `${sciLineString(lhs)} - ${sciLineString(rhs)}`
    );
  });
});

it("works for mul", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    expect(SciLine.mul(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue * rhs.jsValue,
      `${sciLineString(lhs)} * ${sciLineString(rhs)}`
    );
  });
});

it("works for div", () => {
  cartesian([miscValues, miscValues]).map(([lhs, rhs]) => {
    expect(SciLine.div(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue / rhs.jsValue,
      `${sciLineString(lhs)} / ${sciLineString(rhs)}`
    );
  });
});

it("works for pow", () => {
  const powValues = [...range12Values, ...trigValues];

  cartesian([powValues, powValues]).map(([lhs, rhs], i) => {
    expect(SciLine.pow(lhs.sciLineValue, rhs.sciLineValue)).toMatchJsValue(
      lhs.jsValue ** rhs.jsValue,
      `${sciLineString(lhs)} ** ${sciLineString(rhs)}`
    );
  });
});

it("works for sin", () => {
  [...trigValues, ...trigNegativeValues].forEach(v => {
    expect(SciLine.sin(v.sciLineValue)).toMatchJsValue(
      Math.sin(v.jsValue),
      v.title
    );
  });
});

it("works for cos", () => {
  [...trigValues, ...trigNegativeValues].forEach(v => {
    expect(SciLine.cos(v.sciLineValue)).toMatchJsValue(
      Math.cos(v.jsValue),
      v.title
    );
  });
});

it("works for tan", () => {
  [...trigValues, ...trigNegativeValues].forEach(v => {
    expect(SciLine.tan(v.sciLineValue)).toMatchJsValue(
      Math.tan(v.jsValue),
      v.title
    );
  });
});
