type time = [
  | `Second
  | `Minute
  | `Hour
  | `Day
  | `Weekday
  | `Week
  | `Fortnight
  | `Month
  | `Year
  | `Decade
  | `Century
  | `Femtosecond
  | `Picosecond
  | `Nanosecond
  | `Microsecond
  | `Millisecond
];

type length = [
  | `Meter
  | `Inch
  | `Foot
  | `Yard
  | `Mile
  | `League
  | `Fathom
  | `Furlong
  | `LightYear
  | `Parsec
  | `Angstrom
  | `NauticalMile
  | `Femtometer
  | `Picometer
  | `Nanometer
  | `Micrometer
  | `Millimeter
  | `Centimeter
  | `Kilometer
  | `Megameter
  | `Gigameter
  | `Terameter
  | `Petameter
];

type mass = [
  | `Gram
  | `Tonne
  | `Ounce
  | `Pound
  | `Stone
  | `Femtogram
  | `Picogram
  | `Nanogram
  | `Microgram
  | `Milligram
  | `Kilogram
  | `Megagram
  | `Gigagram
  | `Teragram
  | `Petagram
];

type area = [ | `Acre | `Hectare];

type volume = [
  | `Liter
  | `Gallon
  | `USGallon
  | `Quart
  | `Cup
  | `USCup
  | `Teaspoon
  | `Tablespoon
  | `Drop
  | `FluidOunce
  | `Milliliter
  | `Centiliter
];

type energy = [
  | `Joule
  | `Calorie
  | `ElectronVolt
  | `BTU
  | `Therm
  | `Femtojoule
  | `Picojoule
  | `Nanojoule
  | `Microjoule
  | `Millijoule
  | `Centijoule
  | `Kilojoule
  | `Megajoule
  | `Gigajoule
  | `Terajoule
  | `Petajoule
];

type power = [
  | `Watt
  | `Femtowatt
  | `Picowatt
  | `Nanowatt
  | `Microwatt
  | `Milliwatt
  | `Kilowatt
  | `Megawatt
  | `Gigawatt
  | `Terawatt
  | `Petawatt
];

type memory = [
  | `Bit
  | `Byte
  | `Kilobit
  | `Megabit
  | `Gigabit
  | `Terabit
  | `Petabit
  | `Kibibit
  | `Mebibit
  | `Gibibit
  | `Tebibit
  | `Pebibit
  | `Kilobyte
  | `Megabyte
  | `Gigabyte
  | `Terabyte
  | `Petabyte
  | `Kibibyte
  | `Mebibyte
  | `Gibibyte
  | `Tebibyte
  | `Pebibyte
];

type unitless = [ | `Degree | `Radian | `Arcminute | `Arcsecond];

type temperatureLinear = [ | `Kelvin];
type temperatureNonLinear = [ | `Celsius | `Fahrenheit];
type temperature = [ temperatureLinear | temperatureNonLinear];

type unitLinear = [
  time
  | length
  | mass
  | area
  | volume
  | energy
  | power
  | memory
  | unitless
  | temperatureLinear
];
type unitNonLinear = [ temperatureNonLinear];
type unit = [ unitLinear | unitNonLinear];

type unitPower = (unit, int);
type units = list(unitPower);