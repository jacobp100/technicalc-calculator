open Types;

let re = a =>
  switch (a) {
  | `Real(_) => a
  | `Imag(_) => `Zero
  | `Complex(reQ, reC, _, _) => realQC(reQ, reC)
  | _ => `NaN
  };

let im = a =>
  switch (a) {
  | `Real(_) => `Zero
  | `Imag(_) => a
  | `Complex(_, _, imQ, imC) => imagQC(imQ, imC)
  | _ => `NaN
  };
