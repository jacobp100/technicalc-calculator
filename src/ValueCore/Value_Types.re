type percent = [ | `Percent(Scalar.t)];
type matrix = [ | `Matrix(Matrix.t)];
type vector = [ | `Vector(Vector.t)];
type t = [ Scalar.t | percent | matrix | vector | `NaN];
