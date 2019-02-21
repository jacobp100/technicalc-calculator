let safe_mod = (a, b) => Z.rem(a, b) |> Z.add(_, b) |> Z.rem(_, b);
