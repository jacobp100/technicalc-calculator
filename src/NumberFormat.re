type formatting = {
  min_decimal_places: option(int),
  max_decimal_places: option(int),
  digit_separators: bool,
};

let create_format =
    (
      ~min_decimal_places=?,
      ~max_decimal_places=?,
      ~digit_separators=false,
      (),
    ) => {
  min_decimal_places,
  max_decimal_places,
  digit_separators,
};

let _get_decimal_bounds = f =>
  switch (f.min_decimal_places, f.max_decimal_places) {
  | (Some(min), None) => (min, min)
  | (None, Some(max)) => (0, max)
  | (Some(min), Some(max)) => (min, max)
  | (None, None) => (0, 3)
  };

let trim_trailling_zeros = (~start_index=0, ~end_index=?, string) => {
  let end_index = Util.default(String.length(string) - 1, end_index);
  let slice_index = ref(end_index);
  let break = ref(false);
  while (slice_index^ >= start_index && ! break^) {
    switch (string.[slice_index^]) {
    | '0' => slice_index := slice_index^ - 1
    | '.' =>
      slice_index := slice_index^ - 1;
      break := true;
    | _ => break := true
    };
  };

  String.sub(string, 0, slice_index^ + 1)
  ++ String.sub(string, end_index + 1, String.length(string) - 1 - end_index);
};

let add_digit_separators = (~start_index=0, ~end_index=?, string) => {
  let end_index = Util.default(String.length(string), end_index);
  let base_str = ref(string);
  let index = ref(end_index - 3);
  while (index^ > start_index) {
    let len = String.length(base_str^);
    base_str :=
      String.sub(base_str^, 0, index^)
      ++ ","
      ++ String.sub(base_str^, index^, len - index^);
    index := index^ - 3;
  };
  base_str^;
};

let format_integer = (~base=10, formatting, num) => {
  let str = Z.to_string_base(base, num);
  let str =
    if (formatting.digit_separators) {
      add_digit_separators(~start_index=Z.lt(num, Z.zero) ? 1 : 0, str);
    } else {
      str;
    };
  String.uppercase(str);
};

let format_decimal = (~base=10, formatting, num) => {
  let (min_decimal_places, max_decimal_places) =
    _get_decimal_bounds(formatting);

  let abs_num = Q.abs(num);

  let integer =
    format_integer(
      ~base,
      formatting,
      {
        let abs_integer_part = QUtil.floor(abs_num);
        if (Q.lt(num, Q.zero)) {
          Z.neg(abs_integer_part);
        } else {
          abs_integer_part;
        };
      },
    );

  let decimal =
    if (max_decimal_places == 0) {
      "";
    } else if (QUtil.is_int(num)) {
      String.make(min_decimal_places, '0');
    } else {
      let decimal_part = QUtil.safe_mod(abs_num, Z.one);
      let exp = Q.of_bigint(Z.pow(Z.of_int(base), max_decimal_places));
      let decimal_as_integer = QUtil.floor(Q.mul(decimal_part, exp));
      let baseStr =
        Z.to_string_base(base, decimal_as_integer) |> String.uppercase;
      let str =
        String.make(max_decimal_places - String.length(baseStr), '0')
        ++ baseStr;
      trim_trailling_zeros(~start_index=min_decimal_places, str);
    };

  if (decimal != "") {
    integer ++ "." ++ decimal;
  } else {
    integer;
  };
};

let format_exponential = (~base=10, ~exponent=?, formatting, num) => {
  let exponent = Util.default(QUtil.magnitude(num), exponent);
  let decimal_part =
    format_decimal(
      ~base,
      formatting,
      Q.div(num, QUtil.exp_ints(10, exponent)),
    );
  let exponent_part = string_of_int(exponent);
  (decimal_part, exponent_part);
};
