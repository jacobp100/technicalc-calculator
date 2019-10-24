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
  let str = Js.Float.toStringWithRadix(num, ~radix=base);
  let str =
    if (formatting.digitSeparators) {
      addDigitSeparators(~startIndex=num < 0. ? 1 : 0, str);
    } else {
      str;
    };
  String.uppercase(str);
};

let formatDecimal = (~base=10, formatting, num) => {
  let (minDecimalPlaces, maxDecimalPlaces) = _getDecimalBounds(formatting);
  let absNum = abs_float(num);
  let integerPart = floor(absNum);
  let decimalPart = absNum -. integerPart;
  let integer =
    formatInteger(
      ~base,
      formatting,
      num >= 0. ? integerPart : -. integerPart,
    );
  let decimal =
    if (maxDecimalPlaces == 0) {
      "";
    } else if (decimalPart == 0.) {
      String.make(minDecimalPlaces, '0');
    } else {
      let exp = float_of_int(base) ** float_of_int(maxDecimalPlaces);
      let decimalAsInteger = floor(decimalPart *. exp);
      let baseStr =
        Js.Float.toStringWithRadix(decimalAsInteger, ~radix=base)
        ->String.uppercase;
      let str =
        String.make(maxDecimalPlaces - String.length(baseStr), '0')
        ++ baseStr;
      trimTraillingZeros(~startIndex=minDecimalPlaces, str);
    };

  if (decimal != "") {
    integer ++ "." ++ decimal;
  } else {
    integer;
  };
};

let formatExponential = (~base=10, ~exponent=?, formatting, num) => {
  let exponent =
    switch (exponent) {
    | Some(exponent) => exponent
    | None => DecimalUtil.magnitude(num)
    };
  let decimalPart =
    formatDecimal(~base, formatting, num /. 10. ** float_of_int(exponent));
  let exponentPart = string_of_int(exponent);
  (decimalPart, exponentPart);
};