open Types;

let equalScalar = (a: scalar, b: scalar): bool =>
  switch (a, b) {
  | (`Zero, `Zero) => true
  | (`Real(aQ, aC), `Real(bQ, bC))
  | (`Imag(aQ, aC), `Imag(bQ, bC)) => Q.(aQ == bQ) && Constant.(aC == bC)
  | (`Complex(aReQ, aReC, aImQ, aImC), `Complex(bReQ, bReC, bImQ, bImC)) =>
    Q.(aReQ == bReQ && aImQ == bImQ) && Constant.(aReC == bReC && aImC == bImC)
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
