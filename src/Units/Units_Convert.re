open Types;
open Unit_Types;

let linearValue = (unit: unitLinear) =>
  switch (unit) {
  | `Meter => 1.
  | `Inch => 0.0254
  | `Second => 1.
  | `Minute => 60.
  | `Hour => 3600.
  | `Kelvin => 1.
  };

let nonLinearValueToSi = (value, unit: unitNonLinear) =>
  switch (unit) {
  | `Celsius => Decimal.(value + ofFloat(273.15))
  | `Fahrenheit =>
    Decimal.((value - ofFloat(273.15)) * ofFloat(1.8) + ofFloat(32.))
  };

let siToNonLinearValue = (value, unit: unitNonLinear) =>
  switch (unit) {
  | `Celsius => Decimal.(value - ofFloat(273.15))
  | `Fahrenheit =>
    Decimal.((value - ofFloat(32.)) / ofFloat(1.8) + ofFloat(273.15))
  };

let _transformUnits =
    (
      ~transformNonLinearValue,
      ~unitPowerMultiplier,
      value: Decimal.t,
      units: units,
    ) => {
  let rec iterLinearUnits = (value, units) =>
    switch (units) {
    | [(#unitLinear as unit, power), ...tail] =>
      let (_, unitPower) = Unit_Dimensions.ofUnit(unit);
      let nextPower = power * unitPower * unitPowerMultiplier;
      let nextValue =
        Decimal.(
          value * linearValue(unit)->ofFloat ** Decimal.ofInt(nextPower)
        );
      iterLinearUnits(nextValue, tail);
    | [] => value
    | _ => Decimal.nan
    };
  switch (units) {
  | [(#unitNonLinear as unit, 1)] => transformNonLinearValue(value, unit)
  | _ => iterLinearUnits(value, units)
  };
};

let fromSi = (value: value, units) =>
  switch (value) {
  | `Real(re) =>
    let value =
      _transformUnits(
        ~transformNonLinearValue=siToNonLinearValue,
        ~unitPowerMultiplier=-1,
        Real.toDecimal(re),
        units,
      );
    real(Decimal(value));
  | _ => `NaN
  };

let toSi = (value: value, units) =>
  switch (value) {
  | `Real(re) =>
    let value =
      _transformUnits(
        ~transformNonLinearValue=nonLinearValueToSi,
        ~unitPowerMultiplier=1,
        Real.toDecimal(re),
        units,
      );
    real(Decimal(value));
  | _ => `NaN
  };

let convert = (value: value, ~fromUnits, ~toUnits) =>
  if (Unit_Dimensions.unitsCompatible(toUnits, fromUnits)) {
    value->toSi(fromUnits)->fromSi(toUnits);
  } else {
    `NaN;
  };