(*Funzione per il recupero del byte n-esimo della prima round key*)
Restorebyte[k_, t0_, pt0_, ct0_, pt0x_, beta_, gamma_, n_] :=
 Module[{op, resoult, betavec, btilde, t1, delta, pt1, ct1, t1x, pt1x,
    pt0b, pt0xb, pt1b, pt1xb, k0},
   
  op = 0;
  resoult = {};
  betavec = RotateRight[{beta, 0, 0}, n];
  btilde = MixColumns[RotateRows[betavec]];
  t1 = BitXor[t0, PadLeft[PadRight[btilde, 5], 8]];
  t1x = BitXor[t1, {0, 0, gamma, 0, 0, 0, gamma, 0}];
  
  For[delta = 1, delta < 256, delta++,
   
   pt1 = BitXor[pt0, RotateRight[{delta, 0, 0}, n]];
   ct1 = 
    FromHexToByte[
     EncHL24[FromByteToHex[pt1], FromByteToHex[t1], FromByteToHex[k]]];
   pt1x = 
    FromHexToByte[
     DecHL24[FromByteToHex[ct1], FromByteToHex[t1x], 
      FromByteToHex[k]]];
   op += 1;
   
   If[Delete[BitXor[pt0x, pt1x], (n + 1)] == {0, 0}, Null, Continue[]];
   
   pt0b = pt0[[n + 1]];
   pt0xb = pt0x[[n + 1]];
   pt1b = pt1[[n + 1]];
   pt1xb = pt1x[[n + 1]];
   
   If[n == 2,
    pt0xb = BitXor[pt0xb, gamma];
    pt1xb = BitXor[pt1xb, gamma],
    Null];
   
   
   For[k0 = 0, k0 < 256, k0++,
    
    If[((BitXor[Sbox[BitXor[k0, pt0b]], Sbox[BitXor[k0, pt1b]]]) == 
        beta )
      &&
      ((BitXor[Sbox[BitXor[k0, pt0xb]], Sbox[BitXor[k0, pt1xb]]]) == 
        beta),
     Break[], Null]
    ];
   Break[]];
  AppendTo[resoult, {k0, op}];
  resoult
  ]


  (*Implementazione del boomerang attack con recupero della prima round \
key. Oltre al recupero della round key viene anche indicato il numero \
di query all'oracolo che sono state effettuate per raggiungere il \
risultato*)

ClearAll[beta, gamma, pt0, t0, k, t0x, ct0, pt0x, i, operazioni];

(*Inizializzazione variabili scelte in modo randomico*)
beta = RandomInteger[{1, 255}];
gamma = RandomInteger[{1, 255}];
pt0 = FromHexToByte[RandomHexString[6]];
t0 = FromHexToByte[RandomHexString[16]];
k = FromHexToByte[RandomHexString[32]];
t0x = BitXor[t0, {0, 0, gamma, 0, 0, 0, gamma, 0}];
operazioni = 0;

(*Prima cifratura forward*)
ct0 = FromHexToByte[
   EncHL24[FromByteToHex[pt0], FromByteToHex[t0], FromByteToHex[k]]];

(*Prima decifratura backward*)
pt0x = FromHexToByte[
   DecHL24[FromByteToHex[ct0], FromByteToHex[t0x], 
    FromByteToHex[k]]];
operazioni += 1;

data = {
   {"Key", FromByteToHex[k]},
   {"Tweak", FromByteToHex[t0]},
   {"Plaintext", FromByteToHex[pt0]},
   {"Ciphertext", FromByteToHex[ct0]},
   {"Beta", FromByteToHex[{beta}]},
   {"Gamma", FromByteToHex[{gamma}]}
   };

(*Recap variabili*)
Print[Grid[data, Frame -> All, Spacings -> {2, 1}, Alignment -> Left]];

k0 = {};

(*Recupero byte della chiave*)
For[i = 0, i < 3, i++,
 
 byte = Restorebyte[k, t0, pt0, ct0, pt0x, beta, gamma, i];
 AppendTo[k0, byte[[1, 1]]];
 operazioni = operazioni + byte[[1, 2]];
 ]
k0 = FromByteToHex[k0];

Print[""]
Print["Il valore della prima round key è ", k0]
Print["Sono state performate ", operazioni, " operazioni di cifratura \
e ", operazioni, " operazioni di decifratura"]
Print[""]


(*Test per verificare che la chiave recuperata corrisponda a quella \
effettiva generata dal keyschedule*)
k0 == (FromByteToHex /@ 
    KeySchedule[FromByteToHex[k], FromByteToHex[t0]])[[1]]