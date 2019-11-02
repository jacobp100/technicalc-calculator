/*
 This would be much better as a polymorphic variant, but it meant any switch
 statement over values took over 1kb of JS to compile
 */

type unitType =
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
  | Millisecond
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
  | Petameter
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
  | Petagram
  /* Area */
  | Acre
  | Hectare
  /* Volume */
  | Liter
  | Gallon
  | USGallon
  | Quart
  | Cup
  | USCup
  | Teaspoon
  | Tablespoon
  | Drop
  | FluidOunce
  | Milliliter
  | Centiliter
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
  | Petajoule
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
  | Petawatt
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
  | Pebibyte
  /* Temperature */
  | Kelvin
  | Celsius
  | Fahrenheit
  /* Unitless */
  | Degree
  | Radian
  | Arcminute
  | Arcsecond;

type unitPower = (unitType, int);
type units = list(unitPower);