(* ::Package:: *)

Print["load LowMCEncryption"];

SBox[{x0_,x1_,x2_}]:={
x0+x1*x2,
x0+x1+x0*x2,
x0+x1+x2+x0*x1
}

SBoxLayer[state_]:=Join[SBox[state[[1;;3]]],state[[4;;6]]]

LowMCEncrypt[plaintext_,key_,KMatrix_,LMatrix_,RoundConstants_,r_]:=Module[{state},
	state=plaintext+KMatrix . key;
index=1;
	While[index<=r,
		state=SBoxLayer[state];
		state=LMatrix . state;
		state=state+RoundConstants;
		state=state+KMatrix . key;
		index++;
	];
	PolynomialMod[state,2]
]

InverseSBox[{x0_,x1_,x2_}]:={
BitXor[x0,x1,BitAnd[x1,x2]],BitXor[x1,BitAnd[x0,x2]],
BitXor[x0,x1,x2,BitAnd[x0,x1]]
}

MultiplyWithGF2Matrix[matrix_,vector_]:=Mod[matrix . vector,2]
VectorAdd[a_,b_]:=Mod[a+b,2]
InverseSBoxLayer[state_]:=Join[InverseSBox[state[[1;;3]]],state[[4;;6]]];

InverseLMatrix[LMatrix_]:=Mod[Inverse[LMatrix],2];

LowMCDecrypt[ciphertext_,key_,KMatrix_,RoundConstants_,LMatrix_,r_]:=Module[{state},
state=ciphertext;
Do[
state=VectorAdd[state,MultiplyWithGF2Matrix[KMatrix,key]];
state=VectorAdd[state,RoundConstants];
state=MultiplyWithGF2Matrix[InverseLMatrix[LMatrix],state];
state=InverseSBoxLayer[state],
{i,r,1,-1}];
state=VectorAdd[state,MultiplyWithGF2Matrix[KMatrix,key]
];
state
]
