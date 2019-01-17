let format =
    (~minDecimalPlaces=?, ~maxDecimalPlaces=?, ~digitSeparator=false, num) => {
  let (minDecimalPlaces, maxDecimalPlaces) =
    switch (minDecimalPlaces, maxDecimalPlaces) {
    | (Some(d), None)
    | (None, Some(d)) => (d, d)
    | (Some(min), Some(max)) => (min, max)
    | (None, None) => (0, 3)
    };

  let (string, decimalIndex) =
    if (maxDecimalPlaces > 0) {
      let maxPrecision = 12;
      let baseStr = Printf.sprintf("%.12f", num);
      let canRemoveDecimal = minDecimalPlaces == 0;
      let indexOfDecimal = String.length(baseStr) - 1 - maxPrecision;
      let minIndex =
        indexOfDecimal + minDecimalPlaces + (canRemoveDecimal ? (-1) : 0);

      let sliceIndex = ref(indexOfDecimal + maxDecimalPlaces);
      let break = ref(false);
      while (sliceIndex^ > minIndex && ! break^) {
        switch (baseStr.[sliceIndex^]) {
        | '0' => sliceIndex := sliceIndex^ - 1
        | '.' =>
          sliceIndex := sliceIndex^ - 1;
          break := true;
        | _ => break := true
        };
      };

      (String.sub(baseStr, 0, sliceIndex^ + 1), indexOfDecimal);
    } else {
      let baseStr = Printf.sprintf("%.0f", num);
      (baseStr, String.length(baseStr));
    };

  let string =
    if (digitSeparator) {
      let baseStr = ref(string);
      let index = ref(decimalIndex - 3);
      while (index^ > 0) {
        let len = String.length(baseStr^);
        baseStr :=
          String.sub(baseStr^, 0, index^)
          ++ ","
          ++ String.sub(baseStr^, index^, len - index^);
        index := index^ - 3;
      };
      baseStr^;
    } else {
      string;
    };

  string;
};
