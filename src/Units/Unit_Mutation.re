open Unit_Types;

let empty: units = [];

let rec append = ((unit, power) as unitPower: unitPower, units: units) =>
  switch (units) {
  | [(u, p), ...tail] when u == unit =>
    let nextPower = p + power;
    if (nextPower != 0) {
      [(u, nextPower), ...tail];
    } else {
      tail;
    };
  | [otherUnit, ...tail] => [otherUnit, ...append(unitPower, tail)]
  | [] => []
  };