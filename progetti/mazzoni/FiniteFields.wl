(* il nostro campo è quello analogo all' AES *)

p = 2;
f = x^8 + x^4 + x^3 + x + 1;
exp = Exponent[f, x];

Int2Poly[numero_] := IntegerDigits[numero, p, exp] . Reverse@Table[x^i, {i, 0, exp - 1}];
Poly2Int[poly_] := FromDigits[Reverse@CoefficientList[poly, x, exp], p];

(* somma interna al campo*)
FieldPlus[f1_] := f1;
FieldPlus[f1_, f2_] := Poly2Int[PolynomialMod[Int2Poly[f1] + Int2Poly[f2], p]];
FieldPlus[f1_, sf2__] := FieldPlus[f1, FieldPlus[sf2]];

(* prodotto interno al campo*)
FieldTimes[f1_, f2_] := Poly2Int[PolynomialRemainder[Int2Poly[f1]*Int2Poly[f2], f, x, Modulus -> p]];


Print["loaded FiniteFields"];