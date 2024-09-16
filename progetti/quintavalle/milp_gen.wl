(* ::Package:: *)

(*Disugualianze e ugualianze per rappresentare le operazioni di COPY, AND, XOR*)
L[x0_,x1_,x2_,x3_,x4_,y0_,y1_,y2_,y3_,y4_,b_]:={
(*COPY*)
y0==x0,
y1==x1,
y2==x2,
(*H*)
x1+x2>=b,
b>=x1,
b>=x2,
y3==x0+b+x3+x4,
(*COPY*)
y4==x4
}

(*Algoritmo per generare il modello MILP per un round r di update di TRIVIUM*)
GenerateTriviumMILP[r_]:=Module[{M,vars,constr,u0,u,ur,vr,v1,v2,v3},
M={};
vars={};
constr={};

u0=Table[Subscript[Superscript[Symbol["u"],0],i],{i,0,287}];
vars=Join[vars,u0[[1;;80]]];
u=u0;

Do[
vr=Table[Subscript[Superscript[v,j],i],{i,0,287}];
If[j!=r-1,vars=Join[vars,vr[[1;;288]]];];

v1={vr[[66]],vr[[91]],vr[[92]],vr[[93]],vr[[171]]};
AppendTo[vars,Subscript[b1,j]];
If[j!=r-1,vars=Join[vars,v1]];
constr=Join[constr,L[u[[66]],u[[91]],u[[92]],u[[93]],u[[171]],v1[[1]],v1[[2]],v1[[3]],v1[[4]],v1[[5]],Subscript[b1,j]]];
{u[[66]],u[[91]],u[[92]],u[[93]],u[[171]]}=v1;

v2={vr[[162]],vr[[175]],vr[[176]],vr[[177]],vr[[264]]};
AppendTo[vars,Subscript[b2,j]];
If[j!=r-1,vars=Join[vars,v2]];
constr=Join[constr,L[u[[162]],u[[175]],u[[176]],u[[177]],u[[264]],v2[[1]],v2[[2]],v2[[3]],v2[[4]],v2[[5]],Subscript[b2,j]]];
{u[[162]],u[[175]],u[[176]],u[[177]],u[[264]]}=v2;

v3={vr[[243]],vr[[286]],vr[[287]],vr[[288]],vr[[69]]};
AppendTo[vars,Subscript[b3,j]];
If[j!=r-1,vars=Join[vars,v3]];
constr=Join[constr,L[u[[243]],u[[286]],u[[287]],u[[288]],u[[69]],v3[[1]],v3[[2]],v3[[3]],v3[[4]],v3[[5]],Subscript[b3,j]]];
{u[[243]],u[[286]],u[[287]],u[[288]],u[[69]]}=v3;

Do[
If[MemberQ[{66,91,92,93,171,162,175,176,177,264,243,286,287,288,69},h],
Continue[]
];
AppendTo[constr,vr[[h]]==u[[h]]];
,
{h,1,288}
];


u=RotateRight[vr];
,{j,0,r-1}];

AppendTo[M,vars];
AppendTo[M,constr];

M
]
