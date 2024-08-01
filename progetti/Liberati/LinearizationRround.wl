(* ::Package:: *)

(*Get["C:\\Users\\tizia\\Desktop\\Progetto\\LowMCEncryption.wl"];*)
Get["LowMCEncryption.wl"];

Print["load LinearizationRround"];


MakeEquation[equation_,m_]:=Module[{},
eqterms=equation/.Plus->List;
clist=Map[(mm=#/.Times->List;
{Complement[mm,x],Times@@Intersection[x,mm]})&,eqterms];
mterms=Select[clist,#[[2]]==m&];
If[mterms=={},{0,m},{Plus@@Flatten@(Map[First,mterms]/.{}->{1}),m}]
]

mod2simplify[expr_]:=expr/. {
x_*x_:>x/;MemberQ[{x0,x1,x2},x],
x_+x_:>0/;MemberQ[{x0,x1,x2},x]
}

MakeListEquations[equation_]:=Map[MakeEquation[equation,#]&,Map[Times@@#&,Subsets[x]]]


Update0[state_,LMatrix_,RoundConstants_,KMatrix_,key_]:=Module[{resultState},
{left,right}=Partition[state,3];
resultState = PolynomialMod[LMatrix . Flatten[{S0/. Thread[Rule[x,left]],right},1]+RoundConstants+KMatrix . key,2];
resultState];

Update1[state_,LMatrix_,RoundConstants_,KMatrix_,key_]:=Module[{resultState},
{left,right}=Partition[state,3];
resultState = PolynomialMod[LMatrix . Flatten[{S1/. Thread[Rule[x,left]],right},1]+RoundConstants+KMatrix . key,2];
resultState];

StateUpdate[state_, bitsList_, LMatrix_,RoundConstants_,KMatrix_,key_]:=Module[{resultStates}, 
resultStates= {};
maxBitsList=First[Dimensions[bitsList]];
countBitsList = 1;
While [countBitsList <=maxBitsList ,
resultState = state;
bits = bitsList[[countBitsList]];
maxBits = Length[bits];
countBits= 1;
While [countBits <= maxBits,
bit = bits[[countBits]];
If[bit==0,
resultState = Update0[resultState,LMatrix,RoundConstants,KMatrix,key],
resultState =Update1[resultState,LMatrix,RoundConstants,KMatrix,key]];
countBits++;
];
countBitsList++;
resultStates = Append[resultStates,resultState];
];

resultStates
]



Attack[KMatrix2_,LMatrix2_,RoundConstants2_,plaintext2_,r1_,ciphertext2_]:=Module[{},
x={x0,x1,x2};
S=SBox[x]/.{BitXor->Plus,BitAnd->Times};
A=Array[a,{3,3}];
b=Array[c,3];
vars=Union@Flatten@{A,b};
xvars=Join[x,vars];
fieldeqs=xvars^2+xvars;
y={x1*x2,x0*x2,x0*x1};
f=Plus@@y;
expandedEquations=PolynomialMod[(A . x+b)*f+y,fieldeqs,Modulus->2];
equations=mod2simplify/@expandedEquations;
allterms=Flatten[Map[MakeListEquations,equations],1];
system=Select[Union[Map[First[#]==0&,allterms]],#=!=True&];
solution=Solve[system,vars,Modulus->2];
MatrixForm[As=A/.solution[[1]]];
MatrixForm[bs=b/.solution[[1]]];
S1=As . x+bs;
S0=Map[PolynomialMod[#,fieldeqs,Modulus->2]&,S+S1*f];

key=Table[ToExpression["k"<>ToString[i]],{i,0,5}];
plaintext=Table[ToExpression["p"<>ToString[i]],{i,0,5}];

state=plaintext+KMatrix2 . key;
allcombinationBit =  Table[IntegerDigits[i,2,r1],{i,0,2^r1-1}];

Equations = PolynomialMod[StateUpdate[state, allcombinationBit, LMatrix2,RoundConstants2,KMatrix2,key,S0,S1]/.Thread[Rule[plaintext,plaintext2]],2];
guessedkeys=Map[Solve[Thread[#==ciphertext2],key,Modulus->2]&,Equations];
guessdkeys=DeleteCases[guessedkeys,{}];

valori=key/. guessdkeys;


For[i=1, i<=First[Dimensions[valori]], i++,
	espressioni = valori[[i]];
	
	variabili=Variables[espressioni];
	
	combinazioni=Tuples[{0,1},Length[variabili]];
	
	soluzioniValide=Flatten[Map[
	Mod[espressioni/.Thread[variabili->#],2]&,combinazioni],1];
	
	For[j=1,j<=First[Dimensions[soluzioniValide]],j++,
		KeyTry=soluzioniValide[[j]];

		possibileciphertext=LowMCEncrypt[plaintext2,KeyTry,KMatrix2,LMatrix2,RoundConstants2,r1];

			If[possibileciphertext==ciphertext2,
				Print["La chiave trovata \[EGrave]: ",KeyTry];
				Print["ciphertext originale: ",ciphertext2];
				Print["ciphertext con la chiave trovata: ",possibileciphertext];
			]
	]
]
]

