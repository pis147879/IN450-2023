ClearAll[Sbox, InvSbox, AddRoundKey, SubByte, InvSubByte, RotateRows, \
InvRotateRows, MixColumns, InvMixColumns, g, KeySchedule]
(*Sbox ed inversa*)
Sbox[x_] := {99, 124, 119, 123, 242, 107, 111, 197, 48, 1, 103, 43, 
    254, 215, 171, 118, 202, 130, 201, 125, 250, 89, 71, 240, 173, 
    212, 162, 175, 156, 164, 114, 192, 183, 253, 147, 38, 54, 63, 247,
     204, 52, 165, 229, 241, 113, 216, 49, 21, 4, 199, 35, 195, 24, 
    150, 5, 154, 7, 18, 128, 226, 235, 39, 178, 117, 9, 131, 44, 26, 
    27, 110, 90, 160, 82, 59, 214, 179, 41, 227, 47, 132, 83, 209, 0, 
    237, 32, 252, 177, 91, 106, 203, 190, 57, 74, 76, 88, 207, 208, 
    239, 170, 251, 67, 77, 51, 133, 69, 249, 2, 127, 80, 60, 159, 168,
     81, 163, 64, 143, 146, 157, 56, 245, 188, 182, 218, 33, 16, 255, 
    243, 210, 205, 12, 19, 236, 95, 151, 68, 23, 196, 167, 126, 61, 
    100, 93, 25, 115, 96, 129, 79, 220, 34, 42, 144, 136, 70, 238, 
    184, 20, 222, 94, 11, 219, 224, 50, 58, 10, 73, 6, 36, 92, 194, 
    211, 172, 98, 145, 149, 228, 121, 231, 200, 55, 109, 141, 213, 78,
     169, 108, 86, 244, 234, 101, 122, 174, 8, 186, 120, 37, 46, 28, 
    166, 180, 198, 232, 221, 116, 31, 75, 189, 139, 138, 112, 62, 181,
     102, 72, 3, 246, 14, 97, 53, 87, 185, 134, 193, 29, 158, 225, 
    248, 152, 17, 105, 217, 142, 148, 155, 30, 135, 233, 206, 85, 40, 
    223, 140, 161, 137, 13, 191, 230, 66, 104, 65, 153, 45, 15, 176, 
    84, 187, 22}[[x + 1]];


InvSbox[x_] := {82, 9, 106, 213, 48, 54, 165, 56, 191, 64, 163, 158, 
    129, 243, 215, 251, 124, 227, 57, 130, 155, 47, 255, 135, 52, 142,
     67, 68, 196, 222, 233, 203, 84, 123, 148, 50, 166, 194, 35, 61, 
    238, 76, 149, 11, 66, 250, 195, 78, 8, 46, 161, 102, 40, 217, 36, 
    178, 118, 91, 162, 73, 109, 139, 209, 37, 114, 248, 246, 100, 134,
     104, 152, 22, 212, 164, 92, 204, 93, 101, 182, 146, 108, 112, 72,
     80, 253, 237, 185, 218, 94, 21, 70, 87, 167, 141, 157, 132, 144, 
    216, 171, 0, 140, 188, 211, 10, 247, 228, 88, 5, 184, 179, 69, 6, 
    208, 44, 30, 143, 202, 63, 15, 2, 193, 175, 189, 3, 1, 19, 138, 
    107, 58, 145, 17, 65, 79, 103, 220, 234, 151, 242, 207, 206, 240, 
    180, 230, 115, 150, 172, 116, 34, 231, 173, 53, 133, 226, 249, 55,
     232, 28, 117, 223, 110, 71, 241, 26, 113, 29, 41, 197, 137, 111, 
    183, 98, 14, 170, 24, 190, 27, 252, 86, 62, 75, 198, 210, 121, 32,
     154, 219, 192, 254, 120, 205, 90, 244, 31, 221, 168, 51, 136, 7, 
    199, 49, 177, 18, 16, 89, 39, 128, 236, 95, 96, 81, 127, 169, 25, 
    181, 74, 13, 45, 229, 122, 159, 147, 201, 156, 239, 160, 224, 59, 
    77, 174, 42, 245, 176, 200, 235, 187, 60, 131, 83, 153, 97, 23, 
    43, 4, 126, 186, 119, 214, 38, 225, 105, 20, 99, 85, 33, 12, 
    125}[[x + 1]];


(*Xor dello stato con la round key, in questo caso l'inversa è la \
funzione stessa*)
AddRoundKey[x_, roundkey_] := BitXor[x, roundkey];

(*SubByte e la sua inversa*)
SubByte[list_?ListQ] := (Sbox@list);

InvSubByte[list_?ListQ] :=(InvSbox@list);

(*Operazione di rotazione bitwise agente su ogni elemento dello stato \
ed inversa*)
RotateRows[list_?ListQ] := Module[{i, lista, rotate, num},
  lista = {};
  For[i = 0, i < Length[list], i++,
   rotate = RotateLeft[IntegerDigits[list[[i + 1]], 2, 8], i*6];
   num = FromDigits[rotate, 2];
   AppendTo[lista, num]];
  lista
]

InvRotateRows[list_?ListQ] := Module[{i, lista, rotate, num},
  lista = {};
  For[i = 0, i < Length[list], i++,
   rotate = RotateRight[IntegerDigits[list[[i + 1]], 2, 8], i*6];
   num = FromDigits[rotate, 2];
   AppendTo[lista, num]];
  lista
]

(*Operazione di MixColumns agente sul vettore di stato ed inversa*)
MixColumns[list_?ListQ] := Module[{a, i, j, stato, ff, HLmatrix},
  stato = {};
  HLmatrix = {{9, 1, 2}, {2, 9, 1}, {1, 2, 9}};
  For[i = 0, i < 3, i++,
   a = (BitXor @@ 
      Map[(FieldTimes[HLmatrix[[i + 1, #]], list[[#]]]) &, 
       Range[3]]);
   AppendTo[stato, a]
   ];
  stato
]

InvMixColumns[list_?ListQ] := Module[{a, i, j, stato, ff, HLmatrix},
  stato = {};
  HLmatrix = {{6, 8, 39}, {39, 6, 8}, {8, 39, 6}};
  For[i = 0, i < 3, i++,
   a = (BitXor @@ 
      Map[(FieldTimes[HLmatrix[[i + 1, #]], list[[#]]]) &, 
       Range[3]]);
   AppendTo[stato, a]
   ];
  stato
]

(*Funzione usata internamente al keyschedule*)
g[list_, rc_] := BitXor[SubByte[RotateLeft[list]], rc];

(*Definizione del keyschedule*)
KeySchedule[key_?StringQ, tweak_?StringQ] := 
 Module[{klist, twlist, roundkeys, wlist, bylist},
  
  klist = FromHexToByte[key];
  twlist = PadRight[FromHexToByte[tweak], 16];
  bylist = BitXor[klist, twlist];
  wlist = Partition[bylist, 4];
  AppendTo[wlist, 
   Map[BitXor[g[wlist[[4]], {1, 0, 0, 0}][[#]], wlist[[1, #]]] &, 
    Range[4]]];
  AppendTo[wlist, 
   Map[BitXor[wlist[[5, #]], wlist[[2, #]]] &, Range[4]]];
  AppendTo[wlist, 
   Map[BitXor[wlist[[6, #]], wlist[[3, #]]] &, Range[4]]];
  AppendTo[wlist, 
   Map[BitXor[wlist[[7, #]], wlist[[4, #]]] &, Range[4]]];
  AppendTo[wlist, 
   Map[BitXor[g[wlist[[8]], {2, 0, 0, 0}][[#]], wlist[[5, #]]] &, 
    Range[4]]];
  Partition[Take[Flatten[wlist], 33], 3]]

(*esempio di come funziona il keyschedule, check sulla presenza di \
tutte e 11 le round keys e prova su test vector proposto \
dall'articolo*)
rk = KeySchedule["2b7e151628aed2a6abf7158809cf4f3c", "543bd88000017550"];
FromByteToHex /@ rk
Length[rk]



(*Funzione di round di cifratura*)
HfRound[state_, roundkey_] := Module[{stato},
  stato = AddRoundKey[roundkey, state];
  stato = SubByte[stato];
  stato = RotateRows[stato];
  stato = MixColumns[stato];
  stato
  ]


(*Funzione di cifratura Halfloop24*)
EncHL24[plaintext_, tweak_, key_] := 
 Module[{roundkey, by, blocks, numblocks, cipher, state, i, j},
  
  (*Faccio un check sulle dimensioni ed il tipo di dati in input alla \
funzione di modo tale da prevenire errori*)
  If[StringMatchQ[plaintext, HexadecimalCharacter .. ], , 
   Return["Plaintext must be a string of hexadecimal characters"]];
  If[StringMatchQ[tweak, HexadecimalCharacter .. ] && 
    Length[Characters[tweak]] == 16, , 
   Return["Tweak must be a string of hexadecimal characters and \
length 64 bits"]];
  If[StringMatchQ[key, HexadecimalCharacter .. ] && 
    Length[Characters[key]] == 32, , 
   Return["Key must be a string of hexadecimal characters and length \
128 bits"]];
  
  (*Genero le round keys*)
  roundkey = KeySchedule[key, tweak];
  
  (*Trasformo la stringa in input in una lista di byte ed effettuo un \
padding in modo che la lunghezza del 
  plaintext sia divisibile per la lunghezza del blocco*)
  by = FromHexToByte[plaintext];
  by = PadRight[by, Length[by] + Mod[Length[by], 3]];
  
  (*Divido il plaintext in blocchi da 24 bits (3 byte)*)
  blocks = Partition[by, 3];
  numblocks = Length[blocks];
  cipher = {};
  
  (*Eseguo i 10 rounds di cifratura, 
  solo l'ultimo è diverso dagli altri perchè al posto di 
  MixColumns compare due volte AddRoundKey*)
  For[i = 1, i < (Length[blocks] + 1), i++,
   
   state = blocks[[i]];
   (*primi nove round*)
   state = Fold[HfRound, state, Drop[roundkey, -2]];
   (*decimo rounf*)
   state = AddRoundKey[roundkey[[10]], state];
   state = SubByte[state];
   state = RotateRows[state];
   state = AddRoundKey[roundkey[[11]], state];
   
   AppendTo[cipher, state];
   
   ];
  
  (*Converto il risultato in una stringa*)
  FromByteToHex[Flatten[cipher]]
  ]



(*Funzione di round di decifratura*)
HfRoundInv[state_, roundkey_] := Module[{stato},
  stato = InvMixColumns[state];
  stato = InvRotateRows[stato];
  stato = InvSubByte[stato];
  stato = AddRoundKey[roundkey, stato];
  stato
  ]


(*Funzione di decifratura Halfloop24*)
DecHL24[ciphertext_, tweak_, key_] := 
 Module[{roundkey, by, blocks, numblocks, plain, state, i, j},
  
  (*Faccio un check sulle dimensioni ed il tipo di dati in input alla \
funzione di modo tale da prevenire errori*)
  If[StringMatchQ[ciphertext, HexadecimalCharacter .. ], , 
   Return["Plaintext must be a string of hexadecimal characters"]];
  If[StringMatchQ[tweak, HexadecimalCharacter .. ] && 
    Length[Characters[tweak]] == 16, , 
   Return["Tweak must be a string of hexadecimal characters and \
length 64 bits"]];
  If[StringMatchQ[key, HexadecimalCharacter .. ] && 
    Length[Characters[key]] == 32, , 
   Return["Key must be a string of hexadecimal characters and length \
128 bits"]];


 (*Genero le round keys*)
 roundkey = KeySchedule[key, tweak];
 
 (*Trasformo la stringa in input in una lista di byte ed effettuo un \
padding in modo che la lunghezza del 
 plaintext sia divisibile per la lunghezza del blocco*)
 by = FromHexToByte[ciphertext];
 by = PadRight[by, Length[by] + Mod[Length[by], 3]];
 
 (*Divido il plaintext in blocchi da 24 bits*)
 blocks = Partition[by, 3];
 numblocks = Length[blocks];
 plain = {};
 
 (*Eseguo i 10 rounds di decifratura*)
 For[i = 1, i < (Length[blocks] + 1), i++,
   
   state = blocks[[i]];
   (*primo round*)
   state = AddRoundKey[roundkey[[11]], state];
   state = InvRotateRows[state];
   state = InvSubByte[state];
   state = AddRoundKey[roundkey[[10]], state];
   (*ultimi nove round*)
   state = Fold[HfRoundInv, state, Reverse[Drop[roundkey, -2]]];
   AppendTo[plain, state];
   
   ];
 
 (*Converto il risultato in una stringa*)
 FromByteToHex[Flatten[plain]]
 ];


 