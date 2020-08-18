type token =
  | Integer(string)
  | Base2
  | Base8
  | Base16
  | Sqrt
  | Exp
  | Pi
  | E
  | I
  | Dot
  | Plus
  | Minus
  | Slash
  | OpenBracket
  | CloseBracket;

let%private rec iter = (~tokensRev, charList) =>
  switch (charList) {
  | ['0', 'b', ...rest] => append(~tokensRev, rest, Base2)
  | ['0', 'o', ...rest] => append(~tokensRev, rest, Base8)
  | ['0', 'x', ...rest] => append(~tokensRev, rest, Base16)
  | ['s', 'q', 'r', 't', ...rest] => append(~tokensRev, rest, Sqrt)
  | ['e', 'x', 'p', ...rest] => append(~tokensRev, rest, Exp)
  | ['p', 'i', ...rest] => append(~tokensRev, rest, Pi)
  | ['E' | 'e', ...rest] => append(~tokensRev, rest, E)
  | ['i', ...rest] => append(~tokensRev, rest, I)
  | ['.', ...rest] => append(~tokensRev, rest, Dot)
  | ['+', ...rest] => append(~tokensRev, rest, Plus)
  | ['-', ...rest] => append(~tokensRev, rest, Minus)
  | ['/', ...rest] => append(~tokensRev, rest, Slash)
  | ['(', ...rest] => append(~tokensRev, rest, OpenBracket)
  | [')', ...rest] => append(~tokensRev, rest, CloseBracket)
  | [('0'..'9' | 'a'..'f' | 'A'..'F') as char, ...rest] =>
    let char = String.make(1, char);
    let tokensRev =
      switch (tokensRev) {
      | [Integer(existing), ...rest] => [
          Integer(existing ++ char),
          ...rest,
        ]
      | _ => [Integer(char), ...tokensRev]
      };
    iter(~tokensRev, rest);
  | [] => Some(Belt.List.reverse(tokensRev))
  | _ => None
  }
and append = (~tokensRev, charList, token) => {
  iter(~tokensRev=[token, ...tokensRev], charList);
};
let%private tokenize = string =>
  iter(
    ~tokensRev=[],
    Belt.List.makeBy(String.length(string), i => string.[i]),
  );

let%private parseConstant = (~base, tokens) =>
  switch (tokens) {
  | _ when base != None => None
  | [Sqrt, OpenBracket, Integer(sqrt), CloseBracket, ...rest] =>
    switch (Belt.Int.fromString(sqrt)) {
    | Some(sqrt) => Some((Real_Constant.Sqrt(sqrt), rest))
    | None => None
    }
  | [Exp, OpenBracket, Integer(exp), CloseBracket, ...rest] =>
    switch (Belt.Int.fromString(exp)) {
    | Some(exp) => Some((Real_Constant.Exp(exp), rest))
    | None => None
    }
  | [Pi, ...rest] => Some((Pi, rest))
  | rest => Some((Unit, rest))
  };
let%private parseFraction = (~base, tokens) => {
  let (num, constant, tokens) =
    switch (tokens) {
    | [Integer(num), ...rest] =>
      let num = Decimal.ofString(num);
      switch (parseConstant(~base, rest)) {
      | Some((constant, rest)) => (num, Some(constant), Some(rest))
      | None => (num, None, Some(rest))
      };
    | rest =>
      switch (parseConstant(~base, rest)) {
      | Some((constant, rest)) => (Decimal.one, Some(constant), Some(rest))
      | None => (Decimal.nan, None, None)
      }
    };
  let (den, constant, tokens) =
    switch (tokens) {
    | Some([Slash, Integer(den), ...rest]) =>
      let den = Decimal.ofString(den);
      switch (constant) {
      | Some(constant) => (den, Some(constant), Some(rest))
      | None =>
        switch (parseConstant(~base, rest)) {
        | Some((constant, rest)) => (num, Some(constant), Some(rest))
        | None => (num, None, Some(rest))
        }
      };
    | _ => (Decimal.nan, None, None)
    };
  let constant = Belt.Option.getWithDefault(constant, Unit);
  switch (tokens) {
  | Some(tokens) => Some((num, den, constant, tokens))
  | _ => None
  };
};
let%private parseDecimal = (~base, tokens) => {
  let (parsedBase, tokens) =
    switch (base, tokens) {
    | (Some(base), _) => (base, tokens)
    | (None, [Base2, ...rest]) => (2, rest)
    | (None, [Base8, ...rest]) => (8, rest)
    | (None, [Base16, ...rest]) => (16, rest)
    | (_, rest) => (10, rest)
    };
  let (integerStr, decimalString, tokens) =
    switch (tokens) {
    | [Integer(int), Dot, Integer(dec), ...rest] => (
        int,
        Some(dec),
        Some(rest),
      )
    | [Integer(int), Dot, ...rest] => (int, None, Some(rest))
    | [Integer(int), ...rest] => (int, None, Some(rest))
    | _ => ("", None, None)
    };
  let (exp10MagnitudeString, tokens) =
    switch (tokens) {
    | Some([E, Integer(int), ...rest])
    | Some([E, Plus, Integer(int), ...rest]) => (Some(int), Some(rest))
    | Some([E, Minus, Integer(int), ...rest]) => (
        Some("-" ++ int),
        Some(rest),
      )
    | Some(_) as tokens => (None, tokens)
    | None => (None, None)
    };
  let (num, den) =
    if (tokens != None) {
      let (basePrefix, base) =
        switch (parsedBase) {
        | 10 => ("", 10)
        | 2 => ("0b", 2)
        | 8 => ("0o", 8)
        | 16 => ("0x", 16)
        | _ => failwith("invalid base")
        };
      let (num, den) =
        switch (decimalString) {
        | Some(decimalString) => (
            Decimal.(ofString(basePrefix ++ integerStr ++ decimalString)),
            Decimal.(ofInt(base) ** ofInt(String.length(decimalString))),
          )
        | None => (Decimal.ofString(integerStr), Decimal.one)
        };
      let exp10Magnitude =
        Belt.Option.mapWithDefault(
          exp10MagnitudeString,
          Decimal.zero,
          Decimal.ofString,
        );
      switch (Decimal.(cmp(exp10Magnitude, zero))) {
      | 1 => (Decimal.(num * ofInt(10) ** exp10Magnitude), den)
      | (-1) => (num, Decimal.(den * ofInt(10) ** abs(exp10Magnitude)))
      | _ => (num, den)
      };
    } else {
      (Decimal.nan, Decimal.nan);
    };
  switch (tokens) {
  | Some(rest) =>
    switch (parseConstant(~base, rest)) {
    | Some((constant, rest)) => Some((num, den, constant, rest))
    | None => Some((num, den, Unit, rest))
    }
  | None => None
  };
};
let%private parseReal = (~base, tokens) => {
  let state = parseFraction(~base, tokens);
  let state = state == None ? parseDecimal(~base, tokens) : state;
  let state =
    switch (state == None ? parseConstant(~base, tokens) : None) {
    | Some((constant, rest)) =>
      Some((Decimal.one, Decimal.one, constant, rest))
    | None => state
    };
  switch (state) {
  | Some((num, den, constant, rest)) =>
    let value =
      switch (
        Decimal.toFloat(num)->FloatUtil.intValue,
        Decimal.toFloat(den)->FloatUtil.intValue,
      ) {
      | (Some(num), Some(den)) => Real.rational(num, den, constant)
      | _ =>
        Real.decimal(Decimal.(num / den * Real_Constant.toDecimal(constant)))
      };
    Some((value, rest));
  | None => None
  };
};

let%private baseOfString = (~base=?, string) => {
  let tokens = tokenize(string);
  let (positive, tokens) =
    switch (tokens) {
    | Some([Plus, ...rest]) => (true, Some(rest))
    | Some([Minus, ...rest]) => (false, Some(rest))
    | tokens => (true, tokens)
    };
  let (real, tokens) =
    switch (
      positive,
      Belt.Option.flatMap(tokens, tokens => parseReal(~base, tokens)),
    ) {
    | (true, Some((value, tokens))) => (value, Some(tokens))
    | (false, Some((value, tokens))) => (Real.neg(value), Some(tokens))
    | (_, None) => (Real.nan, None)
    };
  switch (tokens) {
  | Some([]) => Some(Types.real(real))
  | Some([I]) => Some(Types.imag(real))
  | Some([(Plus | Minus) as sign, ...rest]) =>
    switch (parseReal(~base, rest)) {
    | Some((imag, [I])) =>
      let imag = sign == Plus ? imag : Real.neg(imag);
      Some(Types.complex(real, imag));
    | _ => None
    }
  | _ => None
  };
};

let ofStringBase = (base, string) => baseOfString(~base, string);
let ofString = string => baseOfString(string);
