


(*implementazione del crittosistema Zorro*)

sbox = "B2E55EFD5FC550BCDC4AFA8828D8E0D1B5D03CB099C1E8E21359A7FB713431\
F19F3ACE6EA8A4B47E1FB7511D389D4669530E421B0F1168CAAA06F0BD266F00D962F3\
1560F23D7F35632D67931C91F99C662A812095F8E34D5A6D247BB9EFDFDA58A992762E\
B3390C29CD43FEABF594231680C0124CE9481908AE41708414A2D5B83365BAED17CF96\
1E3B0BC2C8B6BB8BA15475C4105DD62597E6FC49F75218868DCBE1BFD78E37BE82CC64\
907C328F4BAC1AEAD3F46B2CFF550A45098901302BD2778772EB36DE9E8CDB6C9B0502\
4EAF04AD74C3EEA6F6C77D40D40D3E5BEC78A0B14473475C982122613FC67A56DDE785\
C98A5727079A03A383E46AA52F794F";

S[x_] := IntegerDigits[FromDigits[x, 16], 256, 256]
G[x_] := IntegerDigits[FromDigits[sbox, 16], 256, 256][[x + 1]]

(*S[sbox];*)

text = "FF00000000000000000000000000FF01";
input = IntegerDigits[FromDigits[text, 16], 256, 16];
dom = Range[0, 15];
dom1 = Range[0, 3];

(*SubBytes/SubBytesInverse*)

SubByte[x_] := Map[
  (If[# < 4,
     S[sbox][[x[[# + 1]] + 1]],
     x[[# + 1]]]) & , dom];

(*SubByte[input]*)

(*SubByteInverese*)
SubByteInverse[x_] := Flatten[Map[
   (If[# < 5,
      Position[S[sbox], x[[#]]] - 1,
      x[[#]]]) &, Range[1, 16]
   ]]

(*Shift Rows/ShiftRowsInverse*)
ShiftRiga[i_, x_] := 
 Map[(Partition[x, 4][[i + 1]][[Mod[i + #, 4] + 1]]) &, dom1]
ShiftRows[x_] := Flatten[Map[ShiftRiga[#, x] &, dom1]]

(*ShiftRowsInverse*)

ShiftRigaInverse[i_, x_] := 
 Map[(Partition[x, 4][[i + 1]][[Mod[# - i, 4] + 1]]) &, dom1]
ShiftRowsInverse[x_] := Flatten[Map[ShiftRigaInverse[#, x] &, dom1]]

(* il nostro campo è quello analogo all' AES *)

p = 2;
f = x^8 + x^4 + x^3 + x + 1;
exp = Exponent[f, x];

Int2Poly[numero_] := 
 IntegerDigits[numero, p, exp] . Reverse@Table[x^i, {i, 0, exp - 1}]
Poly2Int[poly_] := FromDigits[Reverse@CoefficientList[poly, x, exp], p]

(* somma interna al campo*)
FieldPlus[f1_] := f1
FieldPlus[f1_, f2_] := 
 Poly2Int[PolynomialMod[Int2Poly[f1] + Int2Poly[f2], p]]
FieldPlus[f1_, sf2__] := FieldPlus[f1, FieldPlus[sf2]]

(* prodotto interno al campo*)
FieldTimes[f1_, f2_] := 
 Poly2Int[
  PolynomialRemainder[Int2Poly[f1]*Int2Poly[f2], f, x, Modulus -> p]]



(*MixColumn/MixColumsInverse*)
matrixcolums = {2, 3, 1, 1, 1, 2, 3, 1, 1, 1, 2, 3, 3, 1, 1, 2};
inversematrixcolums = {14, 11, 13, 9, 9, 14, 11, 13, 13, 9, 14, 11, 
   11, 13, 9, 14};

Elemento[a_, x_, b_, y_] := 
 FieldPlus @@ (Map[
    FieldTimes[Partition[a, 4][[x + 1]][[# + 1]], 
      Transpose[Partition[b, 4]][[y + 1]][[# + 1]]] &, dom1])

Riga[a_, b_, x_] :=
 Map[Elemento[a, x, b, #] &, dom1]

MixColums[a_] :=
 Flatten[Map[Riga[matrixcolums, a, #] &, dom1]]


Partition[MixColums[inversematrixcolums], 4] // MatrixForm;








(*Mix columsInverse*)

MixColumsInverse[a_] :=
 Flatten[Map[Riga[inversematrixcolums, a, #] &, dom1]]


(*Add Key*)



k = "0B00000000000000000E2";
(*key=IntegerDigits[FromDigits[k,16],256,16]*)

key = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};


(*Add Key*)

ADDKey[x_] := Map[FieldPlus[x[[# + 1]], key[[# + 1]]] &, dom]




ADDKey[input];


(* round costante/round costante Inverse*)
AddCostant[text_, i_] :=
 Map[
  If[# < 4,
     If[# < 3, FieldPlus[text[[# + 1]], i], 
      FieldPlus[text[[# + 1]], 8*i]
     ],
    text[[# + 1]]
    
    ] &, dom
  ]






(*stato iniziale e finale *)

firststep[x_] := List[ADDKey[x], 0]
finalstep[x_] := List[ADDKey[x], 24]


(*Round/Round Inverse*)
Raund[x_] := 
 List[MixColums[ShiftRows[AddCostant[SubByte[x[[1]]], x[[2]] + 1]]], 
  x[[2]] + 1]
RaundInverse[x_] := 
 List[SubByteInverse[
   AddCostant[ShiftRowsInverse[MixColumsInverse[x[[1]]]], x[[2]]]], 
  x[[2]] - 1]





(*implementazione 4 round = 1 step*)

State[x_] := 
 List[ADDKey[Nest[Raund, x, 4][[1]]], Nest[Raund, x, 4][[2]]]

State1[x_] := List[Nest[Raund, x, 4][[1]], Nest[Raund, x, 4][[2]]]
InverseState[x_] := 
 List[ADDKey[Nest[RaundInverse, x, 4][[1]]], 
  Nest[RaundInverse, x, 4][[2]]]






(*Ns=numero degli step*)
Ns = 6;
Zorro[x_, i_] := Nest[State, firststep[x], i]

ZorroInverse[x_, i_] := Nest[InverseState, finalstep[x], i]

(*implementazione di Ns steps* di Zorro*)
Zorro[input, 6];




























































