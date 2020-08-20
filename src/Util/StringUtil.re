external charToInt: char => int = "%identity";

let rec splitOnChar = (value, character) =>
  switch (
    Js.String.splitAtMost(
      value,
      ~limit=1,
      Js.String.fromCharCode(charToInt(character)),
    )
  ) {
  | [|head, tail|] => [head, ...splitOnChar(tail, character)]
  | _ => [value]
  };

[@bs.send] external charAtUnsafe: (string, int) => char = "charCodeAt";
