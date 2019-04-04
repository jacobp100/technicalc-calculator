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
  decimal_min_magnitude: float,
  decimal_max_magnitude: float,
};

let default = {
  mode: String,
  style: Natural,
  base: 10,
  precision: 12,
  decimal_min_magnitude: (-3.),
  decimal_max_magnitude: 8.,
};
