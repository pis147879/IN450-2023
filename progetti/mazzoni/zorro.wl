Get["FiniteFields.wl"]

(*implementazione del crittosistema Zorro*)

sbox = "B2E55EFD5FC550BCDC4AFA8828D8E0D1B5D03CB099C1E8E21359A7FB713431\
F19F3ACE6EA8A4B47E1FB7511D389D4669530E421B0F1168CAAA06F0BD266F00D962F3\
1560F23D7F35632D67931C91F99C662A812095F8E34D5A6D247BB9EFDFDA58A992762E\
B3390C29CD43FEABF594231680C0124CE9481908AE41708414A2D5B83365BAED17CF96\
1E3B0BC2C8B6BB8BA15475C4105DD62597E6FC49F75218868DCBE1BFD78E37BE82CC64\
907C328F4BAC1AEAD3F46B2CFF550A45098901302BD2778772EB36DE9E8CDB6C9B0502\
4EAF04AD74C3EEA6F6C77D40D40D3E5BEC78A0B14473475C982122613FC67A56DDE785\
C98A5727079A03A383E46AA52F794F";

lsbox = IntegerDigits[FromDigits[sbox, 16], 256, 256];
S[x_] := S[x] = lsbox[[x + 1]];
SINV[x_] := SINV[x] = Position[lsbox,x][[1,1]]-1;
(*G[x_] := IntegerDigits[FromDigits[sbox, 16], 256, 256][[x + 1]]*)

(*S[sbox];*)

text = "FF00000000000000000000000000FF01";
input = Transpose@Partition[IntegerDigits[FromDigits[text, 16], 256, 16],4];
dom = Range[0, 15];
dom1 = Range[0, 3];

(*SubBytes/SubBytesInverse*)

SubByte[s_] := Module[{state},
    state=s;
    state[[1]]=Map[S,state[[1]]];
    state
]

(*SubByte[input]*)

(*SubByteInverese*)
SubByteInverse[s_] := Module[{state},
    state=s;
    state[[1]]=Map[SINV,state[[1]]];
    state
]

Print[Table[SINV[S[x]]==x,{x,0,255}]]

(*Shift Rows/ShiftRowsInverse*)
ShiftRows[x_] := MapThread[RotateLeft, {x, Range[0, 3]}]

(*ShiftRowsInverse*)
ShiftRowsInverse[x_] := MapThread[RotateRight, {x, Range[0, 3]}];


(*MixColumn/MixColumsInverse*)
matrixcolumns = {{2, 3, 1, 1},{ 1, 2, 3, 1}, {1, 1, 2, 3}, {3, 1, 1, 2}};
inversematrixcolumns = {{14, 11, 13, 9}, {9, 14, 11, 13}, {13, 9, 14, 11}, 
  { 11, 13, 9, 14}};

(*Elemento[a_, x_, b_, y_] := 
 FieldPlus @@ (Map[
    FieldTimes[Partition[a, 4][[x + 1]][[# + 1]], 
      Transpose[Partition[b, 4]][[y + 1]][[# + 1]]] &, dom1])

Riga[a_, b_, x_] :=
 Map[Elemento[a, x, b, #] &, dom1]
*)

MixColumns[state_] := Transpose@Map[Inner[FieldTimes,matrixcolumns,#,FieldPlus]&,Transpose[state]];
MixColumnsInverse[state_] := Transpose@Map[Inner[FieldTimes,inversematrixcolumns,#,FieldPlus]&,Transpose[state]];


MixColumns[inversematrixcolumns] // MatrixForm;

(*Add Key*)

k = "0B00000000000000000E2";
key=Partition[IntegerDigits[FromDigits[k,16],256,16],4]

(*key = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};*)



(*Add Key*)

ADDKey[state_] := MapThread[FieldPlus, {state, key}, 2]

ADDKey[input];


(* round costante/round costante Inverse*)
AddCostant[s_, round_] :=Module[{state},
    state=s;
    state[[1]]=MapThread[FieldPlus,{state[[1]],{round,round,round,BitShiftLeft[round,3]}}];
    state
]

(*stato iniziale e finale *)

FirstStep[x_] := List[ADDKey[x], 0]
FinalStep[x_] := List[ADDKey[x], 24]

(*Round/Round Inverse*)
ZorroRound[x_] := { MixColumns[ShiftRows[AddCostant[SubByte[x[[1]]], x[[2]] + 1]]], x[[2]] + 1}

ZorroRoundInverse[x_] := { SubByteInverse[AddCostant[ShiftRowsInverse[MixColumnsInverse[x[[1]]]] , x[[2]]]], x[[2]] - 1}


(*implementazione 4 round = 1 step*)

State[x_] :=  Module[{state,round},
        {state,round} = Nest[ZorroRound, x, 4];
        {ADDKey[state], round  }
]   
    
StateInverse[x_] :=  Module[{state,round},
        {state,round} = Nest[ZorroRoundInverse, x, 4];
        {ADDKey[state], round  }
]   

(*State1[x_] := List[Nest[Raund, x, 4][[1]], Nest[Raund, x, 4][[2]]]*)

(*Ns=numero degli step*)
Ns = 6;
Zorro[x_, i_] := Nest[State, FirstStep[x], i][[1]]

ZorroInverse[x_, i_] := Nest[StateInverse, FinalStep[x], i]

(*implementazione di Ns steps* di Zorro*)
Print["ciphertext : ",ctx=Zorro[input, Ns]];

Print["decryption : ",ptx=ZorroInverse[ctx, Ns][[1]],"  (",input==ptx,")"];





























































