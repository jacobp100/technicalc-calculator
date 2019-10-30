type mode =
  | String
  | Tex
  | MathML;

type style =
  | Natural
  | Decimal
  | Scientific;

type format = {
  mode,
  style,
  base: int,
  precision: int,
  decimalMinMagnitude: int,
  decimalMaxMagnitude: int,
};

let default = {
  mode: String,
  style: Natural,
  base: 10,
  precision: 12,
  decimalMinMagnitude: (-3),
  decimalMaxMagnitude: 8,
};