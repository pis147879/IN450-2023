ClearAll[p, f, n, x, Int2Poly, Poly2Int, FieldTimes]
(*Caratteristiche del campo*)
p = 2;
n = 8;
f = x^8 + x^4 + x^3 + x + 1;

(*Trasformazione di un elemento del campo nella sua forma polinomiale*)
Int2Poly[numero_] :=
  If[numero >= 0,
   IntegerDigits[numero, p, n] . Reverse@Table[x^i, {i, 0, n - 1}],
   Mod[Times[-1, IntegerDigits[numero, p, n]], p] . 
    Reverse@Table[x^i, {i, 0, n - 1}]];

(*Trasformazione di un polinomio nel corrispondente elemento del \
campo*)
Poly2Int[poly_] :=
  FromDigits[Reverse@CoefficientList[poly, x, n], p];

(*Operazione di moltiplicazione sugli elementi del campo*)
FieldTimes[f1_] := f1;
FieldTimes[f1_, f2_] := 
  Poly2Int[
   PolynomialRemainder[Int2Poly[f1]*Int2Poly[f2], f, x, Modulus -> p]];
FieldTimes[f1_, sf2__] := FieldTimes[f1, FieldTimes[sf2]];
FieldTimes[input_List, input2_?NumericQ] := 
  Map[FieldTimes[input[[#]], input2] &, Range[Length[input]]];
FieldTimes[input1_?NumericQ, input_List] := 
  Map[FieldTimes[input[[#]], input1] &, Range[Length[input]]];
