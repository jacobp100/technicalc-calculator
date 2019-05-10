const cartesian = require("cartesian");
const { range } = require("lodash");
const SciLine = require("../Value.bs");

const testValues = range(-2, 2 + 1, 1);

describe("Quadratic", () => {
  cartesian([testValues, testValues]).forEach(([x0, x1]) =>
    test(`(x - ${x0})(x - ${x1}) = 0`, () => {
      const a = SciLine.ofFloat(1);
      const b = SciLine.ofFloat(-(x0 + x1));
      const c = SciLine.ofFloat(x0 * x1);
      const [r0, r1] = SciLine.quadratic(a, b, c);
      expect([SciLine.toFloat(r0), SciLine.toFloat(r1)].sort()).toEqual(
        [x0, x1].sort()
      );
    })
  );
});

describe("Cubic", () => {
  cartesian([testValues, testValues, testValues]).forEach(([x0, x1, x2]) =>
    test(`(x - ${x0})(x - ${x1})(x - ${x2}) = 0`, () => {
      const a = SciLine.ofFloat(1);
      const b = SciLine.ofFloat(-(x0 + x1 + x2));
      const c = SciLine.ofFloat(x0 * x1 + x0 * x2 + x1 * x2);
      const d = SciLine.ofFloat(-x0 * x1 * x2);
      const [r0, r1, r2] = SciLine.cubic(a, b, c, d);
      expect(
        [SciLine.toFloat(r0), SciLine.toFloat(r1), SciLine.toFloat(r2)].sort()
      ).toEqual([x0, x1, x2].sort());
    })
  );
});
