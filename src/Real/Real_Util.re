let ratDecimal = (n, d, c) =>
  Decimal.(ofInt(n) / ofInt(d) * Real_Constant.toDecimal(c));

let rec gcd = (a, b) => b == 0 ? a : gcd(b, a mod b);
