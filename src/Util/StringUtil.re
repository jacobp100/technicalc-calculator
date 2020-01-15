let rec splitOnChar = (c, v) =>
  switch (String.index(v, c)) {
  | i => [
      String.sub(v, 0, i),
      ...splitOnChar(c, String.sub(v, i + 1, String.length(v) - 1 - i)),
    ]
  | exception Not_found => [v]
  };