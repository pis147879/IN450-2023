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


LinearApproximation[x_]:=Module[{S0,S1},
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
{S0,S1}
]


Update0[state_,S0_]:=Module[{},
{left,right}=Partition[state,3];
PolynomialMod[LMatrix2 . Flatten[{S0/. Thread[Rule[x,left]],right},1]+RoundConstants2+KMatrix2 . key,2]
]

Update1[state_]:=Module[{},
{left,right}=Partition[state,3];
PolynomialMod[LMatrix2 . Flatten[{S1/. Thread[Rule[x,left]],right},1]+RoundConstants2+KMatrix2 . key,2]
]

StateUpdate[approximations_,state_,bit_]:=Module[{},
{S0,S1}=approximations;
If[bit==0, 

Update0[state,S0],
Update0[state,S1]
]]
Clear[SymbolicEncryption];

SymbolicEncryption[approximation_,state_,bitList_]:=Fold[StateUpdate[approximation,#1,#2]&,state,bitList]

ExpandKey[key_]:=Module[{vars,n},
	vars=Variables[key];
	n=Length[vars];
	PolynomialMod[key/.Map[Thread[Rule[vars,IntegerDigits[#,2,n]]]&,Range[0,2^n-1]],2]
]
approximation=LinearApproximation[x];
KeyRecovery[plaintext2_,ciphertext2_,bitList_,approximation_]:=Module[{key,plaintext,solutions},

	key=Table[ToExpression["k"<>ToString[i]],{i,0,5}];
	plaintext=Table[ToExpression["p"<>ToString[i]],{i,0,5}];
	solutions=Solve[Thread[PolynomialMod[SymbolicEncryption[approximation,plaintext+KMatrix2 . key,bitList]/.Thread[Rule[plaintext,plaintext2]],2]==ciphertext2],key,Modulus->2];
	
	If[solutions=={},{},Union@Flatten[Map[ExpandKey,key/.solutions],1]]
	]
Nr=4
keys=Union@Flatten[Map[KeyRecovery[plaintext2,ciphertext2,IntegerDigits[#,2,Nr],approximation]&,Range[0,2^Nr-1]],1]
(*KeyRecovery[plaintext2,ciphertext2,IntegerDigits[1,2,Nr],approximation]*)
findkey=Map[{#,LowMCEncrypt[plaintext2,#,KMatrix2,LMatrix2,RoundConstants2,Nr]}&,keys]
Select[findkey,#[[2]]==ciphertext2&]


(* ::Input:: *)
(*Variables[{{k0,0,1,1+k0,1+k0,0}}]*)
