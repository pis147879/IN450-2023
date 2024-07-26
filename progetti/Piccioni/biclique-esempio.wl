<<"biclique.wl"

baseKey = RandomInteger[{0,15}, {4,4}];
baseKey[[1, 2]]=0; baseKey[[4, 1]]=0;

Oracle[ciphertexts_]:= Module[{secretKey, plaintexts},
	secretKey = StringToMatrix["F0D0DEADBEEFF00D"];
	plaintexts = Table[DecryptionAES[ciphertexts[[i]], secretKey], {i,1,16}];
	Return[plaintexts];
]

Print["chiave random : ",BicliqueAttack[baseKey]];

baseKey = RandomInteger[{0,15}, {4,4}];
baseKey[[1, 2]]=0;
baseKey[[4, 1]]=0;

Oracle[ciphertexts_]:= Module[{secretKey, plaintexts},
	secretKey = (PrecomputedKeys[baseKey])[[1, 1]];
	(*Print[secretKey];*) (* Verifica che la chiave segreta è corretta *)
	plaintexts = Table[DecryptionAES[ciphertexts[[i]], secretKey], {i,1,16}];
	Return[plaintexts];
]


Print["chiave nel gruppo K[i,j] : ",BicliqueAttack[baseKey]]