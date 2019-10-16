open Types;

let equalScalar = (a: scalar, b: scalar): bool =>
  switch (a, b) {
  | (`Zero, `Zero) => true
  | (`Real(a), `Real(b))
  | (`Imag(a), `Imag(b)) => Real.(a == b)
  | (`Complex(aRe, aIm), `Complex(bRe, bIm)) =>
    Real.(aRe == bRe && aIm == bIm)
  | _ => false
  };

let equal = (a: value, b: value): bool =>
  switch (a, b) {
  | (`Zero, `Zero) => true
  | (
      (`Real(_) | `Imag(_) | `Complex(_)) as aV,
      (`Real(_) | `Imag(_) | `Complex(_)) as bV,
    ) =>
    equalScalar(aV, bV)
  | (`Percent(aP), `Percent(bP)) => equalScalar(aP, bP)
  | (`Vector(aElements), `Vector(bElements))
      when Belt.Array.length(aElements) == Belt.Array.length(bElements) =>
    Belt.Array.every2(aElements, bElements, equalScalar)
  | (`Matrix(aM), `Matrix(bM))
      when aM.numRows == bM.numRows && aM.numColumns == bM.numColumns =>
    Belt.Array.every2(aM.elements, bM.elements, equalScalar)
  | _ => false
  };