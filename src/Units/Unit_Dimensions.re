open Unit_Types;

type dimension =
  | Length
  | Time
  | Temperature;

module Comparable =
  Belt.Id.MakeComparable({
    type t = dimension;
    let cmp = (a, b) => Pervasives.compare(a, b);
  });
type dimensionMap('value) =
  Belt.Map.t(Comparable.t, 'value, Comparable.identity);

let ofUnit = (v: unit) =>
  switch (v) {
  | #length => (Length, 1)
  | #time => (Time, 1)
  | #temperature => (Temperature, 1)
  };

let baseDimensions = (units: units) =>
  Belt.List.reduce(
    units,
    Belt.Map.make(~id=(module Comparable)),
    (map, (unit, power)) => {
      let (dimension, dimensionPower) = ofUnit(unit);
      Belt.Map.update(
        map,
        dimension,
        current => {
          let value =
            Belt.Option.getWithDefault(current, 0) + power * dimensionPower;
          value != 0 ? Some(value) : None;
        },
      );
    },
  );

let powEq = (aPow: int, bPow: int) => aPow == bPow;

let unitsCompatible = (a: units, b: units) =>
  Belt.Map.eq(baseDimensions(a), baseDimensions(b), powEq);