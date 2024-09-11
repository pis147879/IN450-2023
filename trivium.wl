(* ::Package:: *)

h[a_,b_,c_,d_,e_]:=Mod[a+b*c+d+e,2];

(*Preso un registro di 288 bit fa un round di update*)
TriviumUpdate[x_]:=Module[{xp},
xp=x;
xp[[93]]=Apply[h,xp[[{66,91,92,93,171}]]];
xp[[177]]=Apply[h,xp[[{162,175,176,177,264}]]];
xp[[288]]=Apply[h,xp[[{243,286,287,288,69}]]];

RotateRight[xp]
]

(*Genera lo stato iniziale con 80 bit di chiave e 80 di input*)
TriviumSetup[key_,IV_]:=Join[key,Table[0,13],IV,{0,0,0,0},Table[0,108],{1,1,1}];

(*Genera il bit di chiave di output*)
TriviumOutput[s_]:=PolynomialMod[Apply[Plus,s[[{0,93,177}+1]]],2]

(*Genera il keystream di output partendo da un vettore inizializzato*)
TriviumKeystream[key_,IV_,r_,len_]:=Map[TriviumOutput,NestList[TriviumUpdate,Nest[TriviumUpdate,TriviumSetup[key,IV],r],len]]
