open Unit_Types;

type dimension =
  | Length
  | Time
  | Mass
  | Memory
  | Temperature;

module Comparable =
  Belt.Id.MakeComparable({
    type t = dimension;
    let cmp = (a, b) => Pervasives.compare(a, b);
  });
type dimensionMap('value) =
  Belt.Map.t(Comparable.t, 'value, Comparable.identity);

let lengthDimensions = [(Length, 1)];
let timeDimensions = [(Time, 1)];
let massDimensions = [(Mass, 1)];
let areaDimensions = [(Length, 2)];
let volumeDimensions = [(Length, 3)];
let energyDimensions = [(Mass, 1), (Length, 2), (Time, (-2))];
let powerDimensions = [(Mass, 1), (Length, 2), (Time, (-3))];
let memoryDimensions = [(Memory, 1)];
let unitlessDimensions = [];
let temperatureDimensions = [(Temperature, 1)];

let unitDimensions = (v: unit) =>
  switch (v) {
  | #length => lengthDimensions
  | #time => timeDimensions
  | #mass => massDimensions
  | #area => areaDimensions
  | #volume => volumeDimensions
  | #energy => energyDimensions
  | #power => powerDimensions
  | #memory => memoryDimensions
  | #unitless => unitlessDimensions
  | #temperature => temperatureDimensions
  };

let baseDimensions = (units: units) => {
  let addUnit = (map, (unit, power)) => {
    let addDimension = (map, (dimension, dimensionPower)) =>
      Belt.Map.update(
        map,
        dimension,
        current => {
          let value =
            Belt.Option.getWithDefault(current, 0) + power * dimensionPower;
          value != 0 ? Some(value) : None;
        },
      );

    Belt.List.reduce(unitDimensions(unit), map, addDimension);
  };

  Belt.List.reduce(units, Belt.Map.make(~id=(module Comparable)), addUnit);
};

let powEq = (aPow: int, bPow: int) => aPow == bPow;

let unitsCompatible = (a: units, b: units) =>
  Belt.Map.eq(baseDimensions(a), baseDimensions(b), powEq);