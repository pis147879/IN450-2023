(* ::Package:: *)

Get["C:\\Users\\tizia\\Desktop\\Progetto\\LowMCEncryption.wl"];

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


Update0[state_]:=Module[{},
{left,right}=Partition[state,3];
PolynomialMod[LMatrix2 . Flatten[{S0/. Thread[Rule[x,left]],right},1]+RoundConstants2+KMatrix2 . key,2]
]

Update1[state_]:=Module[{},
{left,right}=Partition[state,3];
PolynomialMod[LMatrix2 . Flatten[{S1/. Thread[Rule[x,left]],right},1]+RoundConstants2+KMatrix2 . key,2]
]

StateUpdate[state_,bit_]:=If[bit==0, 
Update0[state],
Update1[state]
]
Clear[SymbolicEncryption];
SymbolicEncryption[state_,bitList_]:=Fold[StateUpdate,state,bitList]

ExpandKey[key_]:=Module[{vars,n},
	vars=Variables[key];
	n=Length[vars];
	PolynomialMod[key/.Map[Thread[Rule[vars,IntegerDigits[#,2,n]]]&,Range[0,2^n-1]],2]
]

KeyRecovery[plaintext2_,ciphertext2_,bitList_]:=Module[{key,plaintext,solutions},
	key=Table[ToExpression["k"<>ToString[i]],{i,0,5}];
	plaintext=Table[ToExpression["p"<>ToString[i]],{i,0,5}];
	solutions=Solve[Thread[PolynomialMod[SymbolicEncryption[plaintext+KMatrix2 . key,bitList]/.Thread[Rule[plaintext,plaintext2]],2]==ciphertext2],key,Modulus->2];
	
	If[solutions=={},{},Union@Flatten[Map[ExpandKey,key/.solutions],1]]
	]
Nr=4
keys=Union@Flatten[Map[KeyRecovery[plaintext2,ciphertext2,IntegerDigits[#,2,Nr]]&,Range[0,2^Nr-1]],1]
KeyRecovery[plaintext2,ciphertext2,IntegerDigits[15,2,Nr]]
findkey=Map[{#,LowMCEncrypt[plaintext2,#,KMatrix2,LMatrix2,RoundConstants2,Nr]}&,keys]
Select[findkey,#[[2]]==ciphertext2&]


(* ::Input:: *)
(*Variables[{{k0,0,1,1+k0,1+k0,0}}]*)


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

SymbolicEncryptionOne=SymbolicEncryption[state,r1];
Print[SymbolicEncryptionOne];

Equations=PolynomialMod[SymbolicEncryptionOne/.Thread[Rule[plaintext,plaintext2]],2];

guessedkeys=Solve[Thread[Equations==ciphertext2],key,Modulus->2];


]



