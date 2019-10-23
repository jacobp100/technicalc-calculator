type formatting = {
  minDecimalPlaces: option(int),
  maxDecimalPlaces: option(int),
  digitSeparators: bool,
};

let createFormat =
    (~minDecimalPlaces=?, ~maxDecimalPlaces=?, ~digitSeparators=false, ()) => {
  minDecimalPlaces,
  maxDecimalPlaces,
  digitSeparators,
};

let _getDecimalBounds = f =>
  switch (f.minDecimalPlaces, f.maxDecimalPlaces) {
  | (Some(min), None) => (min, min)
  | (None, Some(max)) => (0, max)
  | (Some(min), Some(max)) => (min, max)
  | (None, None) => (0, 3)
  };

let trimTraillingZeros = (~startIndex=0, ~endIndex=?, string) => {
  let endIndex =
    endIndex->Belt.Option.getWithDefault(String.length(string) - 1);
  let sliceIndex = ref(endIndex);
  let break = ref(false);
  while (sliceIndex^ >= startIndex && ! break^) {
    switch (string.[sliceIndex^]) {
    | '0' => sliceIndex := sliceIndex^ - 1
    | '.' =>
      sliceIndex := sliceIndex^ - 1;
      break := true;
    | _ => break := true
    };
  };

  String.sub(string, 0, sliceIndex^ + 1)
  ++ String.sub(string, endIndex + 1, String.length(string) - 1 - endIndex);
};

let addDigitSeparators = (~startIndex=0, ~endIndex=?, string) => {
  let endIndex = endIndex->Belt.Option.getWithDefault(String.length(string));
  let baseStr = ref(string);
  let index = ref(endIndex - 3);
  while (index^ > startIndex) {
    let len = String.length(baseStr^);
    baseStr :=
      String.sub(baseStr^, 0, index^)
      ++ ","
      ++ String.sub(baseStr^, index^, len - index^);
    index := index^ - 3;
  };
  baseStr^;
};

let formatInteger = (~base=10, formatting, num) => {
  // let str = string_of_int(num);
  // let str =
  //   if (formatting.digitSeparators) {
  //     addDigitSeparators(~startIndex=num < 0 ? 1 : 0, str);
  //   } else {
  //     str;
  //   };
  "...";
  // String.uppercase(str);
};

let formatDecimal = (~base=10, formatting, num) => {
  // let (minDecimalPlaces, maxDecimalPlaces) = _getDecimalBounds(formatting);
  // let absNum = Q.abs(num);
  // let integer =
  //   formatInteger(
  //     ~base,
  //     formatting,
  //     {
  //       let absIntegerPart = absNum;
  //       if (Q.lt(num, Q.zero)) {
  //         - absIntegerPart;
  //       } else {
  //         absIntegerPart;
  //       };
  //     },
  //   );
  // let decimal =
  //   if (maxDecimalPlaces == 0) {
  //     "";
  //   } else if (QUtil.isInt(num)) {
  //     String.make(minDecimalPlaces, '0');
  //   } else {
  //     let decimalPart = QUtil.safeMod(absNum, Z.one);
  //     let exp = Q.of_bigint(Z.pow(Z.of_int(base), maxDecimalPlaces));
  //     let decimalAsInteger = QUtil.floor(Q.mul(decimalPart, exp));
  //     let baseStr =
  //       Z.to_string_base(base, decimalAsInteger)->String.uppercase;
  //     let str =
  //       String.make(maxDecimalPlaces - String.length(baseStr), '0')
  //       ++ baseStr;
  //     trimTraillingZeros(~startIndex=minDecimalPlaces, str);
  //   };
  // if (decimal != "") {
  //   integer ++ "." ++ decimal;
  // } else {
  //   integer;
  "...";
  // };
};

let formatExponential = (~base=10, ~exponent=?, formatting, num) => {
  // let exponent = exponent->Belt.Option.getWithDefault(QUtil.magnitude(num));
  // let decimalPart =
  //   formatDecimal(
  //     ~base,
  //     formatting,
  //     Q.div(num, QUtil.powInt(10, exponent)),
  //   );
  // let exponentPart = string_of_int(exponent);
  "...";
  // (decimalPart, exponentPart);
};