type token =
  | Integer(string)
  | NaN
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
  | CloseBracket
  | OpenBrace
  | CloseBrace
  | Comma;

let%private rec iter = (~tokensRev, charList) =>
  switch (charList) {
  | ['N', 'a', 'N', ...rest]
  | ['n', 'a', 'n', ...rest] => append(~tokensRev, rest, NaN)
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
  | ['{', ...rest] => append(~tokensRev, rest, OpenBrace)
  | ['}', ...rest] => append(~tokensRev, rest, CloseBrace)
  | [',', ...rest] => append(~tokensRev, rest, Comma)
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
  | [' ', ...rest] => iter(~tokensRev, rest)
  | [] => Some(Belt.List.reverse(tokensRev))
  | _ => None
  }
and append = (~tokensRev, charList, token) => {
  iter(~tokensRev=[token, ...tokensRev], charList);
};
let%private tokenize = string =>
  iter(
    ~tokensRev=[],
    Belt.List.makeBy(String.length(string), i =>
      StringUtil.charAtUnsafe(string, i)
    ),
  );

let%private partialParseConstant = (~base, tokens) =>
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
let%private partialParseFraction = (~base, tokens) => {
  let tokens = base != None ? None : Some(tokens);
  let (num, constant, tokens) =
    switch (tokens) {
    | Some([Integer(num), ...rest]) =>
      let num = Decimal.ofString(num);
      switch (partialParseConstant(~base, rest)) {
      | Some((constant, rest)) => (num, Some(constant), Some(rest))
      | None => (num, None, Some(rest))
      };
    | Some(rest) =>
      switch (partialParseConstant(~base, rest)) {
      | Some((constant, rest)) => (Decimal.one, Some(constant), Some(rest))
      | None => (Decimal.nan, None, None)
      }
    | None => (Decimal.nan, None, None)
    };
  let (den, constant, tokens) =
    switch (tokens) {
    | Some([Slash, Integer(den), ...rest]) =>
      let den = Decimal.ofString(den);
      switch (constant) {
      | Some(constant) => (den, Some(constant), Some(rest))
      | None =>
        switch (partialParseConstant(~base, rest)) {
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
    | [Dot, Integer(dec), ...rest] => ("0", Some(dec), Some(rest))
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
        | None => (Decimal.ofString(basePrefix ++ integerStr), Decimal.one)
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
    switch (partialParseConstant(~base, rest)) {
    | Some((constant, rest)) => Some((num, den, constant, rest))
    | None => Some((num, den, Unit, rest))
    }
  | None => None
  };
};
let%private partialparseReal = (~base, tokens) => {
  let state = partialParseFraction(~base, tokens);
  let state = state == None ? parseDecimal(~base, tokens) : state;
  let state =
    switch (state == None ? partialParseConstant(~base, tokens) : None) {
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

let%private parseScalar = (~base, tokens): option(Types.scalar) => {
  let (positive, tokens) =
    switch (tokens) {
    | [Minus, ...rest] => (false, rest)
    | [Plus, ...rest] => (true, rest)
    | tokens => (true, tokens)
    };
  let (real, tokens) =
    switch (positive, partialparseReal(~base, tokens)) {
    | (true, Some((value, tokens))) => (value, Some(tokens))
    | (false, Some((value, tokens))) => (Real.neg(value), Some(tokens))
    | (_, None) => (Real.nan, None)
    };
  switch (tokens) {
  | Some([(Plus | Minus) as sign, ...rest]) =>
    switch (partialparseReal(~base, rest)) {
    | Some((imag, [I])) =>
      let imag = sign == Plus ? imag : Real.neg(imag);
      Some(`Complex((real, imag))->Types.normalizeScalar);
    | _ => None
    }
  | Some([I]) => Some(`Imag(real)->Types.normalizeScalar)
  | Some([]) => Some(`Real(real)->Types.normalizeScalar)
  | _ => None
  };
};

// TODO: Test
let%private parseTokens = (~base, tokens) => {
  let rec iter = tokens =>
    switch (tokens) {
    | [OpenBrace, ...rest] => parseUntilCloseBrace(rest)
    | rest =>
      switch (parseScalar(~base, rest)) {
      | Some(scalar) => Some((`Scalar(scalar), []))
      | None => None
      }
    }
  and parseUntilCloseBrace =
      (~tokensAccumRev=[], ~elementsRev=[], ~level=0, tokens) => {
    switch (tokens) {
    | [CloseBrace, ...rest] when level == 0 =>
      let elementsRev =
        switch (iter(Belt.List.reverse(tokensAccumRev))) {
        | Some((closedElement, [])) => Some([closedElement, ...elementsRev])
        | _ => None
        };
      switch (elementsRev) {
      | Some([scalar]) => Some((scalar, rest))
      | Some(elementsRev) =>
        Some((`Row(Belt.List.reverse(elementsRev)), rest))
      | None => None
      };
    | [Comma, ...rest] when level == 0 =>
      switch (iter(Belt.List.reverse(tokensAccumRev))) {
      | Some((element, [])) =>
        parseUntilCloseBrace(
          ~tokensAccumRev=[],
          ~elementsRev=[element, ...elementsRev],
          ~level,
          rest,
        )
      | _ => None
      }
    | [] => None
    | [CloseBrace as e, ...rest] =>
      parseUntilCloseBrace(
        ~tokensAccumRev=[e, ...tokensAccumRev],
        ~elementsRev,
        ~level=level - 1,
        rest,
      )
    | [OpenBrace, ..._] as e => iter(e)
    | [e, ...rest] =>
      parseUntilCloseBrace(
        ~tokensAccumRev=[e, ...tokensAccumRev],
        ~elementsRev,
        ~level,
        rest,
      )
    };
  };

  let ast =
    switch (iter(tokens)) {
    | Some((ast, [])) => Some(ast)
    | _ => None
    };

  switch (ast) {
  | Some(`Scalar(scalar)) => Some(Types.valueOfScalar(scalar))
  | Some(`Row([`Scalar(a), `Scalar(b)]))
  | Some(`Row([`Row([`Scalar(a)]), `Row([`Scalar(b)])])) =>
    Some(Types.vector2(a, b))
  | Some(`Row([`Scalar(a), `Scalar(b), `Scalar(c)]))
  | Some(
      `Row([`Row([`Scalar(a)]), `Row([`Scalar(b)]), `Row([`Scalar(c)])]),
    ) =>
    Some(Types.vector3(a, b, c))
  | Some(
      `Row([
        `Row([`Scalar(a), `Scalar(b)]),
        `Row([`Scalar(c), `Scalar(d)]),
      ]),
    ) =>
    Some(Types.matrix2(a, b, c, d))
  | Some(
      `Row([
        `Row([`Scalar(a), `Scalar(b), `Scalar(c)]),
        `Row([`Scalar(d), `Scalar(e), `Scalar(f)]),
        `Row([`Scalar(g), `Scalar(h), `Scalar(i)]),
      ]),
    ) =>
    Some(Types.matrix3(a, b, c, d, e, f, g, h, i))
  | _ => None
  };
};

let tempOfStringBase = (~base, string) => {
  let scalar =
    switch (tokenize(string)) {
    | Some(tokens) => parseScalar(~base, tokens)
    | None => None
    };
  switch (scalar) {
  | Some(scalar) => Some(Types.valueOfScalar(scalar))
  | None => None
  };
};

let ofStringBase = (base, string) =>
  tempOfStringBase(~base=Some(base), string);
let ofString = string => tempOfStringBase(~base=None, string);
