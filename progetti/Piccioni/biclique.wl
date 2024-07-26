




(* Input e output sono matrici 4x4 *)

(* Ricordiamo che:
RoundFunction[state_, roundKey_] := AddKey[MixColumns[ShiftRows[SubBytes[state]]], roundKey];
*)

(* Definizione della funzione h per i round 1-3 *)
h[P_, key_]:= Module[{roundKey, state},
	roundKey = ExpKey[key];
	state = AddKey[P, roundKey[[1]]];
	state = Fold[RoundFunction, state, Take[roundKey, {2,4}]];
	Return[state];
]

(* Definizione della funzione g per i round 4-8 *)
g[V_, key_]:= Module[{roundKey, state},
	roundKey = ExpKey[key];
	state = V;
	state = Fold[RoundFunction, state, Take[roundKey, {5,9}]];
	Return[state];
]

(* Definizione della funzione f per i round 9-10 *)
f[S_, key_]:= Module[{roundKey, state},
	roundKey = ExpKey[key];
	state = S;
	state = Fold[RoundFunction, state, Take[roundKey, {10}]];
	state = AddKey[ShiftRows[SubBytes[state]], roundKey[[-1]]];
	Return[state];
]

(* Input e output sono matrici 4x4 *)

(* Ricordiamo che:
InvRoundFunction[state_, roundKey_]:= InvSubBytes[InvShiftRows[InvMixColumns[InvAddKey[state, roundKey]]]];
*)

(* Definizione dell'inversa di g *)
gInv[S_, key_]:= Module[{roundKey, state},
	roundKey = ExpKey[key];
	state = S;
	state = Fold[InvRoundFunction, state, Take[Reverse[roundKey], {3,7}]];
	Return[state];
]

(* Definizione dell'inversa di f *)
fInv[V_, key_]:= Module[{roundKey, state},
	roundKey = ExpKey[key];
	state = V;
	state = InvSubBytes[InvShiftRows[InvAddKey[state, roundKey[[-1]]]]];
	state = Fold[InvRoundFunction, state, Take[roundKey, {10}]];
	Return[state];
]


Differentials[]:= Module[{deltaK, nablaK},
	deltaK = Table[{{0,0,0,0},{0,0,0,0},{i,0,0,0},{i,0,0,0}}, {i,0,15}];
	nablaK = Table[{{0,j,0,0},{0,0,0,0},{0,j,0,0},{0,0,0,0}}, {j,0,15}];
	{deltaK, nablaK}
]

RelatedKeyDifferentials[baseKey_] := Module[{deltaK, nablaK, Ki0, K0j},
	{deltaK, nablaK} = Differentials[];
	Ki0 = Table[BitXor[baseKey, deltaK[[i]]], {i,1,16}];
	K0j = Table[BitXor[baseKey, nablaK[[j]]], {j,1,16}];
	{Ki0, K0j}
]

PrecomputedKeys[baseKey_] := Module[{deltaK, nablaK, Kij},
	{deltaK, nablaK} = Differentials[];
	Kij = Table[BitXor[baseKey, deltaK[[i]], nablaK[[j]]], {j,1,16}, {i,1,16}];
	Return[Kij];
]

BicliqueConstruction[S0_, C0_] := Module[{deltaK, nablaK, intermediateStates, ciphertexts},
	{deltaK, nablaK} = Differentials[];
	intermediateStates = Table[BitXor[S0, nablaK[[j]]], {j,1,16}];
	ciphertexts = Table[BitXor[C0, deltaK[[i]]], {i,1,16}];
	{intermediateStates, ciphertexts}
]



PrecomputedStates[plaintexts_, intermediateStates_, baseKey_]:= Module[{Ki0, K0j, preForwardStates, preBackwardStates},
	{Ki0, K0j} = RelatedKeyDifferentials[baseKey];
	preForwardStates = Table[h[plaintexts[[i]], Ki0[[i]]], {i,1,16}];
	preBackwardStates = Table[gInv[intermediateStates[[j]], K0j[[j]]], {j,1,16}];
	{preForwardStates, preBackwardStates}
]

RecomputedStates[preForwardStates_, preBackwardStates_] := Module[{deltaK, nablaK, forwardStates, backwardStates},
	{deltaK, nablaK} = Differentials[];
	forwardStates = Table[BitXor[preForwardStates[[i]], nablaK[[j]]], {j,1,16}, {i,1,16}];
	backwardStates = Table[BitXor[preBackwardStates[[j]], deltaK[[i]]], {i,1,16}, {j,1,16}];
	{forwardStates, backwardStates}
]

FindMatchingState[forwardStates_, backwardStates_] := Module[{match},
	match = Intersection[Flatten[forwardStates, 1], Flatten[backwardStates, 1]];
	(* Se esiste almeno un match, e quindi la lista è non vuota, la funzione restituisce il primo elemento della lista *)
	If[match === {}, None, match[[1]]]
]

KeyRecovery[forwardStates_, backwardStates_, baseKey_] := Module[{matchingState, indexi, indexj, candidateKey},
	candidateKey = None;
	matchingState = FindMatchingState[forwardStates, backwardStates];
	If[matchingState === None,
		None,
		(* else *)
		{indexi, indexj} = Flatten[Position[forwardStates, matchingState]]; (* con backwardStates è lo stesso *)
		candidateKey = BitXor[baseKey, {{0,0,0,0},{0,0,0,0},{indexi-1,0,0,0},{indexi-1,0,0,0}}, {{0,indexj-1,0,0},{0,0,0,0},{0,indexj-1,0,0},{0,0,0,0}}]
	];
	Return[candidateKey]
]


BicliqueAttack[baseKey_] := Module[{S0, C0, intermediateStates, ciphertexts, plaintexts, preForwardStates, preBackwardStates, forwardStates, backwardStates, recoveredKey},
	(* Costruzione del biclique *)
	C0 = StringToMatrix["45AE67EDD574633A"];
	S0 = fInv[C0, baseKey];
	{intermediateStates, ciphertexts} = BicliqueConstruction[S0, C0];
	(* Matching con le pre-computazioni *)
	plaintexts = Oracle[ciphertexts]; (* la funzione Oracle è definita negli esempi *)
	{preForwardStates, preBackwardStates} = PrecomputedStates[plaintexts, intermediateStates, baseKey];
	{forwardStates, backwardStates} = RecomputedStates[preForwardStates, preBackwardStates];
	(* Recupero della chiave *)
	recoveredKey = KeyRecovery[forwardStates, backwardStates, baseKey];
	If[recoveredKey === None,
		Print["Chiave segreta non trovata"],
		Print["La chiave segreta è: ", recoveredKey]
	];
]
