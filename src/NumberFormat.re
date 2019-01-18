type formatting = {
  min_decimal_places: option(int),
  max_decimal_places: option(int),
  digit_separator: bool,
};

let create_format =
    (~min_decimal_places=?, ~max_decimal_places=?, ~digit_separator=false, ()) => {
  min_decimal_places,
  max_decimal_places,
  digit_separator,
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
  while (slice_index^ > start_index && ! break^) {
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
  let end_index = Util.default(String.length(string) - 1, end_index);
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

let format_decimal = (formatting, num) => {
  let (min_decimal_places, max_decimal_places) =
    _get_decimal_bounds(formatting);

  let (string, decimal_index) =
    if (max_decimal_places > 0) {
      let precision = 12;
      let str = Printf.sprintf("%.12f", num);
      let index_of_decimal = String.length(str) - 1 - precision;
      let str = String.sub(str, 0, index_of_decimal + max_decimal_places + 1);
      let can_remove_decimal = min_decimal_places == 0;
      let min_index =
        index_of_decimal + min_decimal_places + (can_remove_decimal ? (-1) : 0);
      (trim_trailling_zeros(~start_index=min_index, str), index_of_decimal);
    } else {
      let str = Printf.sprintf("%.0f", num);
      (str, String.length(str));
    };

  let string =
    if (formatting.digit_separator) {
      add_digit_separators(~end_index=decimal_index, string);
    } else {
      string;
    };

  string;
};

let format_exponential = (~exponent=?, ~exponent_format="e$", formatting, num) => {
  let num_magnitude = num == 0. ? 0. : floor(log10(abs_float(num)));
  let exponent = Util.default(num_magnitude, exponent);
  let decimal_part = format_decimal(formatting, num /. 10. ** exponent);
  let exponent_part =
    format_decimal(create_format(~max_decimal_places=0, ()), exponent);
  let formatted_exponent = {
    let index = String.index(exponent_format, '$');
    String.sub(exponent_format, 0, index)
    ++ exponent_part
    ++ String.sub(
         exponent_format,
         index,
         String.length(exponent_format) - 1 - index,
       );
  };
  decimal_part ++ formatted_exponent;
};
