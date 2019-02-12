let default = (defaultValue, arg) =>
  switch (arg) {
  | Some(v) => v
  | None => defaultValue
  };

let rec string_split_on_char = (c, v) =>
  switch (String.index(v, c)) {
  | i => [
      String.sub(v, 0, i),
      ...string_split_on_char(
           c,
           String.sub(v, i + 1, String.length(v) - 1 - i),
         ),
    ]
  | exception Not_found => [v]
  };
