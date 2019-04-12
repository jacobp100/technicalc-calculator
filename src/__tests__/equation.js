const cartesian = require("cartesian");
const { range } = require("lodash");
const Complex = require("../Scalar/Complex.bs");
const Equation = require("../Equation.bs");

const testValues = range(-2, 2 + 1, 1);

describe("Quadratic", () => {
  cartesian([testValues, testValues]).forEach(([x0, x1]) =>
    test(`(x - ${x0})(x - ${x1}) = 0`, () => {
      const a = Complex.ofInt(1);
      const b = Complex.ofInt(-(x0 + x1));
      const c = Complex.ofInt(x0 * x1);
      const [r0, r1] = Equation.quadratic(a, b, c);
      expect([Complex.toInt(r0), Complex.toInt(r1)].sort()).toEqual(
        [x0, x1].sort()
      );
    })
  );
});

describe("Cubic", () => {
  cartesian([testValues, testValues, testValues]).forEach(([x0, x1, x2]) =>
    test(`(x - ${x0})(x - ${x1})(x - ${x2}) = 0`, () => {
      const a = Complex.ofInt(1);
      const b = Complex.ofInt(-(x0 + x1 + x2));
      const c = Complex.ofInt(x0 * x1 + x0 * x2 + x1 * x2);
      const d = Complex.ofInt(-x0 * x1 * x2);
      const [r0, r1, r2] = Equation.cubic(a, b, c, d);
      expect(
        [Complex.toInt(r0), Complex.toInt(r1), Complex.toInt(r2)].sort()
      ).toEqual([x0, x1, x2].sort());
    })
  );
});
