<<"FiniteFields.wl"

(* Definizione della funzione f *)
ff[x_]:= If[x==0, 0, FongInverse[x]];
TableForm@Table[{a,ff[a]}, {a,0,15}];

(* Definizione della funzione g *)
(* Costruiamo A come una matrice circolante generata dal vettore v=(1,1,0,1) *)
CirculantL[v_]:= Map[RotateLeft[v, #]&, Range[Length@v]-1];
A = CirculantL[{1,1,0,1}];
MatrixForm[A];
Det[A, Modulus->2] (* Verifichiamo che det(A) è non nullo in F_2 *)
(* Determiniamo b in modo che g non abbia punti fissi *)
b = {b0,b1,b2,b3};
x = {x0,x1,x2,x3};
Solve[A . x+b==x, x, Modulus->2]
b = {1,1,0,1}; (* Le componenti di b sono scelte in accordo con le condizioni date dalla riga precedente *)
Solve[A . x+b==x, x, Modulus->2] (* Verifichiamo che b non crea punti fissi *)
gg[x_]:= FromDigits[Mod[A . IntegerDigits[x, 2, Length@A]+b, 2], 2];

(* Definizione della funzione f *)
ff[x_]:= If[x==0, 0, FongInverse[x]];
TableForm@Table[{a,ff[a]}, {a,0,15}];

(* Definizione della funzione g *)
(* Costruiamo A come una matrice circolante generata dal vettore v=(1,1,0,1) *)
CirculantL[v_]:= Map[RotateLeft[v, #]&, Range[Length@v]-1];
A = CirculantL[{1,1,0,1}];
MatrixForm[A];
Det[A, Modulus->2] (* Verifichiamo che det(A) è non nullo in F_2 *)
(* Determiniamo b in modo che g non abbia punti fissi *)
b = {b0,b1,b2,b3};
x = {x0,x1,x2,x3};
Solve[A . x+b==x, x, Modulus->2]
b = {1,1,0,1}; (* Le componenti di b sono scelte in accordo con le condizioni date dalla riga precedente *)
Solve[A . x+b==x, x, Modulus->2] (* Verifichiamo che b non crea punti fissi *)
gg[x_]:= FromDigits[Mod[A . IntegerDigits[x, 2, Length@A]+b, 2], 2];

(* Definizione della funzione S_RD *)
SRD[x_]:= gg[ff[x]];
TableForm@Table[{a,SRD[a]}, {a,0,15}];

(* Definizione della funzione SubBytes *)
SubBytes[state_]:= Map[SRD, state, {2}]; (* Utilizziamo Map a livello 2 per applicare SRD a ogni componente della matrice state *)

Invgg[x_]:= FromDigits[Mod[A . (IntegerDigits[x, 2, Length@A]+b), 2], 2];

InvSRD[x_]:= ff[Invgg[x]];
TableForm@Table[{a,SRD[a],InvSRD[SRD[a]]}, {a,0,15}] (* Verifica della correttezza di InvSRD *)
InvSubBytes[state_]:= Map[InvSRD, state, {2}];


ShiftRows[state_]:= MapThread[RotateLeft[#1, #2]&, {state, Range[0,3]}]; (* #1 si riferisce a state, #2 a Range[0,3] *)
InvShiftRows[state_]:= MapThread[RotateRight[#1, #2]&, {state, Range[0,3]}];


