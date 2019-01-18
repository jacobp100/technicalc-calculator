type mode =
  | String
  | Latex;

type style =
  | Natural
  | Numerical
  | Scientific;

type format = {
  mode,
  style,
  precision: int,
};

let default = {mode: String, style: Natural, precision: 12};
