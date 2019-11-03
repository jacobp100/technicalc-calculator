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

let timeDimensions = [|(Time, 1)|];
let lengthDimensions = [|(Length, 1)|];
let massDimensions = [|(Mass, 1)|];
let areaDimensions = [|(Length, 2)|];
let volumeDimensions = [|(Length, 3)|];
let energyDimensions = [|(Mass, 1), (Length, 2), (Time, (-2))|];
let powerDimensions = [|(Mass, 1), (Length, 2), (Time, (-3))|];
let memoryDimensions = [|(Memory, 1)|];
let temperatureDimensions = [|(Temperature, 1)|];

let unitDimensions = (v: unitType) =>
  switch (v) {
  /* Time */
  | Second
  | Minute
  | Hour
  | Day
  | Week
  | Month
  | Year
  | Decade
  | Century
  | Femtosecond
  | Picosecond
  | Nanosecond
  | Microsecond
  | Millisecond => timeDimensions
  /* Length */
  | Meter
  | Inch
  | Foot
  | Yard
  | Mile
  | LightYear
  | Parsec
  | Angstrom
  | Femtometer
  | Picometer
  | Nanometer
  | Micrometer
  | Millimeter
  | Centimeter
  | Kilometer
  | Megameter
  | Gigameter
  | Terameter
  | Petameter => lengthDimensions
  /* Mass */
  | Gram
  | Tonne
  | Ounce
  | Pound
  | Stone
  | Femtogram
  | Picogram
  | Nanogram
  | Microgram
  | Milligram
  | Kilogram
  | Megagram
  | Gigagram
  | Teragram
  | Petagram => massDimensions
  /* Area */
  | Acre
  | Hectare => areaDimensions
  /* Volume */
  | Liter
  | Gallon
  | USGallon
  | Quart
  | Cup
  | USCup
  | Teaspoon
  | Tablespoon
  | FluidOunce
  | Milliliter
  | Centiliter => volumeDimensions
  /* Energy */
  | Joule
  | Calorie
  | ElectronVolt
  | BTU
  | Therm
  | Femtojoule
  | Picojoule
  | Nanojoule
  | Microjoule
  | Millijoule
  | Centijoule
  | Kilojoule
  | Megajoule
  | Gigajoule
  | Terajoule
  | Petajoule => energyDimensions
  /* Power */
  | Watt
  | Femtowatt
  | Picowatt
  | Nanowatt
  | Microwatt
  | Milliwatt
  | Kilowatt
  | Megawatt
  | Gigawatt
  | Terawatt
  | Petawatt => powerDimensions
  /* Memory */
  | Bit
  | Byte
  | Kilobit
  | Megabit
  | Gigabit
  | Terabit
  | Petabit
  | Kibibit
  | Mebibit
  | Gibibit
  | Tebibit
  | Pebibit
  | Kilobyte
  | Megabyte
  | Gigabyte
  | Terabyte
  | Petabyte
  | Kibibyte
  | Mebibyte
  | Gibibyte
  | Tebibyte
  | Pebibyte => memoryDimensions
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit => temperatureDimensions
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

    Belt.Array.reduce(unitDimensions(unit), map, addDimension);
  };

  Belt.Array.reduce(units, Belt.Map.make(~id=(module Comparable)), addUnit);
};

let powEq = (aPow: int, bPow: int) => aPow == bPow;

let unitsCompatible = (a: units, b: units) =>
  Belt.Map.eq(baseDimensions(a), baseDimensions(b), powEq);