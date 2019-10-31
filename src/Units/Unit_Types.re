type length = [ | `Meter | `Inch];

type time = [ | `Second | `Minute | `Hour];

type temperatureLinear = [ | `Kelvin];
type temperatureNonLinear = [ | `Celsius | `Fahrenheit];
type temperature = [ temperatureLinear | temperatureNonLinear];

type unitLinear = [ length | time | temperatureLinear];
type unitNonLinear = [ temperatureNonLinear];
type unit = [ unitLinear | unitNonLinear];

type unitPower = (unit, int);
type units = list(unitPower);