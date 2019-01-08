const cartesian = require("cartesian");
const SciLine = require("../SciLine.bs");

class Value {
  constructor(value, title = String(value)) {
    this.title = title;
    this.sciLineValue = SciLine.of_real(value);
    this.value = value;
  }
}

const values = [new Value(0), new Value(1), new Value(2)];

it("works for add", () => {
  cartesian([values, values]).map(([lhs, rhs]) => {
    const sciLineValue = SciLine.float_of_value(
      SciLine.add(lhs.sciLineValue, rhs.sciLineValue)
    );
    const value = lhs.value + rhs.value;
    expect(sciLineValue).toEqual(value);
  });
});
