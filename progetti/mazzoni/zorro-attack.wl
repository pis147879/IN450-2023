
<< "zorro.wl"
(*attacco differenziale Zorro* per trovare 32 bit, 4 byte*)

dom2 = Range[0, 2^8 - 1];

(*costruiamo la DDT*)

RowDDT[f_, dx_] := Map[(BitXor[f[BitXor[#, dx]], +f[#]]) &, dom2];
RowDDT2[f_, dx_] := Map[{dx + 1, #[[1]] + 1} -> #[[2]] &, Tally[RowDDT[f, dx]]];
sparseDDT[f_] := Flatten@Map[RowDDT2[f, #] &, dom2];
DDT[f_] := Normal[SparseArray[sparseDDT[f]]];
Print[DDT[G] // MatrixForm];
sparseDDT[G];

d = Map[DDT[G][[#, #]] &, Range[1, 256]];

(*la posizione dei valori più alti ,6, della DDT per cui la \
caratteristica dell'input = caratteristica dell'output nella S-Box, \
le verificano tre elementi*)

Position[d, 6] - 1

(*generazione degli 8 plaintext adeguati e le 12 relative coppie a \
partire da un testo generato casualmente*)


dax1 = {0, 0, 0, 0, 136, 158, 136, 158, 22, 22, 22, 22, 149, 175, 149, 175};
dax2 = {0, 0, 0, 0, 92, 55, 92, 55, 107, 107, 107, 107, 143, 50, 143,  50};
dax3 = {0, 0, 0, 0, 22, 78, 22, 78, 88, 88, 88, 88, 98, 138, 98, 138};
dax4 = {0, 0, 0, 0, 158, 136, 158, 136, 22, 22, 22, 22, 175, 149, 175, 149};
dax5 = {0, 0, 0, 0, 55, 92, 55, 92, 107, 107, 107, 107, 50, 143, 50, 143};
dax6 = {0, 0, 0, 0, 78, 22, 78, 22, 88, 88, 88, 88, 138, 98, 138, 98};

dc1 = {0, 123, 0, 123, 85, 136, 85, 136, 0, 35, 0, 35, 42, 131, 42, 131};
dc2 = {0, 234, 0, 234, 168, 92, 168, 92, 0, 93, 0, 93, 113, 228, 113, 228};
dc3 = {0, 247, 0, 247, 79, 22, 79, 22, 0, 140, 0, 140, 168, 58, 168, 58};
dc4 = {123, 0, 123, 0, 136, 85, 136, 85, 35, 0, 35, 0, 131, 42, 131,  42};
dc5 = {234, 0, 234, 0, 92, 168, 92, 168, 93, 0, 93, 0, 228, 113, 228, 113};
dc6 = {247, 0, 247, 0, 22, 79, 22, 79, 140, 0, 140, 0, 58, 168, 58, 168};


day1 = {0, 123, 0, 123, 136, 85, 136, 85, 0, 35, 0, 35, 131, 42, 131,  42};
day2 = {0, 234, 0, 234, 92, 168, 92, 168, 0, 93, 0, 93, 228, 113, 228, 113};
day3 = {0, 247, 0, 247, 22, 79, 22, 79, 0, 140, 0, 140, 58, 168, 58,  168};
day4 = {123, 0, 123, 0, 85, 136, 85, 136, 35, 0, 35, 0, 42, 131, 42, 131};
day5 = {234, 0, 234, 0, 168, 92, 168, 92, 93, 0, 93, 0, 113, 228, 113, 228};
day6 = {247, 0, 247, 0, 79, 22, 79, 22, 140, 0, 140, 0, 168, 58, 168, 58};

sets1 = Drop[Subsets[{dax1, dax2, dax3}, 3], 1];
sets2 = Drop[Subsets[{dax4, dax5, dax6}, 3], 1];

SUM[string_, x_] := BitXor[string, BitXor @@ x]
PlaintextStructure[string_, sets_] :=  Map[SUM[string, sets[[#]]] &, Range[1, 7]]

(*genero le coppie di plaintext che verificano le condizioni delle \
caratterisitiche*)


index = {4, 6, 5, 4, 6};
Pairs[string_, x_, y_] := 
 If[x < 4, List[string, PlaintextStructure[string, y][[x]]],
  If[x < 10,
   If[EvenQ[x],
    List[  PlaintextStructure[string, y][[x/2 - 1]], 
     PlaintextStructure[string, y][[index[[x/2 - 1]]]]],
    List[PlaintextStructure[string, y][[(x - 1)/2 - 1]], 
     PlaintextStructure[string, y][[index[[(x - 1)/2 + 1]]]]]
    ], List[PlaintextStructure[string, y][[x - 6]], 
    PlaintextStructure[string, y][[7]]
    ]
   ]
  ]

PlaintextsGenerator[string_, y_] := 
 Map[Pairs[string, #, y] &, Range[1, 12]]


n = 5;

GeneratorPlaintext[x_, y_] := 
 Table[PlaintextsGenerator[RandomInteger[255, 16], y] , x]

(*l'ordine delle caratteristiche delle coppie è \
(dx1,dx2,dx3,dx2,dx3,dx3,dx1,dx1,dx2,dx3,dx2,dx1)*)

l = GeneratorPlaintext[n, sets1];

(*per ogni coppia generiamo il corrispondente chipertext*)
step = 1;
CoppiaChiper[l_, x_, y_, step_] :=  Map[MixColumsInverse[Zorro[l[[x, y, #]], step][[1]]] &, Range[1, 2]]
StrutturaChiper[x_, n_, step_] :=  Map[CoppiaChiper[x, n, #, step] &, Range[1, 12]]
ChipertextPairs[x_, n_, step_] :=  Map[StrutturaChiper[x, #, step] &, Range[1, n]]

c = ChipertextPairs[l, n, step];

(*ordino le coppie rispetto alle 3 possibili caratteristiche*)

OrdinaChar[x_, n_] := 
 List[List[x[[n, 1]], x[[n, 7]], x[[n, 8]], x[[n, 12]]], 
  List[x[[n, 2]], x[[n, 4]], x[[n, 9]], x[[n, 11]]], 
  List[x[[n, 3]], x[[n, 5]], x[[n, 6]], x[[n, 10]]]]

RightChar[x_, n_] := Map[OrdinaChar[x, #] &, Range[1, n]]

caratteristiche = Transpose[RightChar[c, n]];

(*ho ordinato le coppie rispetto alle caratteristiche cosichè se \
prendo caratteristiche[[1]] ottengo tutte le coppie che hanno la \
caratteristica di x1/x4 *)

(*scegliamo ora le coppie con le caratteristiche che ci interessano*)

s1 = List[dc1, dc2, dc3];
s2 = List[dc4, dc5, dc6];
d1 = List[day1, day2, day3];
d2 = List[day4, day5, day6];
i = 1;
dy = d1;
s = s1;


TestChar[x_, n_, i_, d_, s_] := Plus @@ (Map[
    If[BitXor[x[[i, n, d]][[1]], x[[i, n, d]][[2]]][[#]] == 
       s[[i, #]],
      1, 0] &, Range[1, 16]])



CoppieChar[x_, n_, i_, s_] := 
 Map[TestChar[x, n, i, #, s] &, Range[1, 4]]

ScelteCoppie[x_, i_, s_, n_] := 
 Map[CoppieChar[x, #, i, s] &, Range[1, n]]

(*per ogni coppia verifichiamo la cartteristica, se soddisfa \
l'ugualianza delle ultime 3 righe e la presenza di 2 0 nella prima \
allora le scegliamo*)

cp = Flatten[
   List[ScelteCoppie[caratteristiche, 1, dy, n], 
    ScelteCoppie[caratteristiche, 2, dy, n], 
    ScelteCoppie[caratteristiche, 3, dy, n]], 1];

(*scegliamo la prima coppia, se non ci sono coppie che verificano la \
condizione allora prendiamo {} *)

rightcupple = 
 First[Flatten[
    List[Position[cp, 14 ], Position[cp, 15], Position[cp, 16]], 
    1] /. {} -> Nothing, {}]

(*la posizione della caratteristica ,  position=1,2,3  *)
position = 
  If[rightcupple[[1]] < n + 1, 1, 
   If[n < rightcupple[[1]] < 2 n + 1, 2, 3
    ]];

(*sto prendendo la coppia di plaintext che verifica la caratteristica \
cercata*)

a = caratteristiche[[position, rightcupple[[1]] - n (position - 1), 
   rightcupple[[2]]]];

(*l'algoritmo che racchiude tutti i procedimenti elencati. troviamo \
due coppie che verificano le caratteristiche, l'implementazione \
dell'algoritmo è più avanti*)

coppie = Trova2Coppie[n, 1, sets1, d1, "a"];

(*proviamo tutte le 2^16 porzioni di chiave sulle coppie rimaste*)

DIFF1[x_] := 
 Table[BitXor[
   x, {0, a, 0, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}], {a, 0, 
   255}, {b, 0, 255}]
DIFF2[x_] := 
 Table[BitXor[
   x, {a, 0, b, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}], {a, 0, 
   255}, {b, 0, 255}]
DIFF1[caratteristiche[[i, 1, 1, 1]]][[1, 1]];

DIFF = DIFF1;

CharKey[x_, y_, step_] := 
 Table[BitXor[
   SubByteInverse[AddCostant[ShiftRowsInverse[x[[a, b]]], 4*step]], 
   SubByteInverse[
    AddCostant[ShiftRowsInverse[y[[a, b]]], 4*step]]], {a, 1, 
   256}, {b, 1, 256}]

testers[x_, DIFF_, step_] := CharKey[DIFF[x[[1]]], DIFF[x[[2]]], step];

TrovaChar[x_, DIFF_, step_] := 
 Map[testers[x[[#]], DIFF, step] &, Range[1, 2]]

charposition = List[TrovaChar[coppie[[1]], DIFF1, step], coppie[[2]]];

charposition[[1, 1, 1]];

(*vediamo se con una data porzione di chiave di 2^16 bit è verificata \
la caratteristica all'inizio del Round dove è presente le S-Box \
Attive*)

TrovaChiave[x_, y_] := Plus @@ (Map[
    If[x[[#]] == y[[#]], 1
      , 0
      ] &, Range[1, 16]])
Chiavi[x_, y_] := 
 Table[TrovaChiave[x[[a, b]], y], {a, 1, 256}, {b, 1, 256}]


ChiaviComuni[x_, y_, position_] := 
 Map[Position[Chiavi[x[[#]], y[[position[[#]]]]], 16] &, Range[1, 2]]



chiavifinale = 
  Flatten[Cases[
      Tally[Flatten[
        ChiaviComuni[charposition[[1]], s1, charposition[[2]]], 
        1]], {_, 2}], 1][[1]] - 1;






(*l'algoritmo che genera, a partire da n strutture, 12n coppie di \
plaintext che verificano le proprietà delle caratteristiche, genera \
12n coppie di chiperintext e verifica le proprietà delle \
caratteristiche, se soddisfano le condizioni ci dà la coppia che \
verifica la condizione, altrimenti ci restituisce {}*)

TrovaCoppia[n_, step_, sets_, dy_] := 
 Module[{l, c, caratteristiche, cp, rightcupple, position, a1},
  l = GeneratorPlaintext[n, sets];
  c = ChipertextPairs[l, n, step];
  caratteristiche = Transpose[RightChar[c, n]];
  cp = Flatten[
    List[ScelteCoppie[caratteristiche, 1, dy, n], 
     ScelteCoppie[caratteristiche, 2, dy, n], 
     ScelteCoppie[caratteristiche, 3, dy, n]], 1];
  rightcupple = 
   First[Flatten[
      List[Position[cp, 14 ], Position[cp, 15], Position[cp, 16]], 
      1] /. {} -> Nothing, {}];
  If[rightcupple === {}, {},
   position = 
    If[rightcupple[[1]] < n + 1, 1, 
     If[n < rightcupple[[1]] < 2 n + 1, 2, 3
      ]];
   a1 = caratteristiche[[position, 
     rightcupple[[1]] - n (position - 1), rightcupple[[2]]]];
   List[a1, position]
   ]
  ]





(*l'algoritmo che cerca due coppie che verificano le condizioni delle \
caratteristiche richieste per poter cercare la chiave equivalente*)



Trova2Coppie[n_, step_, sets_, dy_, a_] := Module[{h, c, i},
  i = 1;
  h = {{, }, {, }};
  While[h[[1, 2]] === Null,
   c = TrovaCoppia[n, step, sets, dy];
   If[c === {}, h,
    If[h[[1, 1]] === Null,
     Print["trovata prima coppia" ];
     h[[1, 1]] = c[[1]];
     h[[2, 1]] = c[[2]],
     h[[1, 2]] = c[[1]];
     h[[2, 2]] = c[[2]];
     Print["trovata seconda coppia"]
     ]
    ];
   Print[ciclofinito_, a _, i];
   i++;
   ];
  h
  ]

















(*algoritmo per cercare 32 bit della chiave equivalente rispetto allo \
step i-esimo. Più step ci sono e più strutture di testi abbiamo \
bisogno*)



Trova32BitChiave [n_, step_] := 
 Module[{coppie1, coppie2, charposition1, charposition2, 
   chiavifinale1, chiavifinale2, a, b},
  coppie1 = Trova2Coppie[n, step, sets1, d1, "a"];
  coppie2 = Trova2Coppie[n, step, sets2, d2, "b"];
  charposition1 = 
   List[TrovaChar[coppie1[[1]], DIFF1, step], coppie1[[2]]];
  charposition2 = 
   List[TrovaChar[coppie2[[1]], DIFF2, step], coppie2[[2]]];
  
  chiavifinale1 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition1[[1]], s1, charposition1[[2]]], 
         1]], {_, 2}], 1][[1]] - 1;
  
  chiavifinale2 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition2[[1]], s2, charposition2[[2]]], 
         1]], {_, 2}], 1][[1]] - 1;
  List[chiavifinale2[[1]], chiavifinale1[[1]], chiavifinale2[[2]], 
   chiavifinale1[[2]]]
  
  ]



















(*Ricerca di altri 32 bit di chiave*)
(*le caratteristiche utili per la ricerca di altri 32 bit, 4 bytes, \
utili*)

dbx1 = {0, 0, 0, 0, 178, 164, 178, 164, 88, 0, 88, 0, 205, 175, 205, 
   175};
dbx2 = {0, 0, 0, 0, 225, 138, 225, 138, 183, 0, 183, 0, 56, 50, 56, 
   50};
dbx3 = {0, 0, 0, 0, 254, 166, 254, 166, 123, 0, 123, 0, 25, 138, 25, 
   138};
dbx4 = {0, 0, 0, 0, 164, 178, 164, 178, 0, 88, 0, 88, 175, 205, 175, 
   205};
dbx5 = {0, 0, 0, 0, 138, 225, 138, 225, 0, 183, 0, 183, 50, 56, 50, 
   56};
dbx6 = {0, 0, 0, 0, 166, 254, 166, 254, 0, 123, 0, 123, 138, 25, 138, 
   25};

dby1 = {0, 0, 0, 0, 158, 136, 158, 136, 22, 22, 22, 22, 175, 149, 175,
    149};
dby2 = {0, 0, 0, 0, 55, 92, 55, 92, 107, 107, 107, 107, 50, 143, 50, 
   143};
dby3 = {0, 0, 0, 0, 78, 22, 78, 22, 88, 88, 88, 88, 138, 98, 138, 
   98};
dby4 = {0, 0, 0, 0, 136, 158, 136, 158, 22, 22, 22, 22, 149, 175, 149,
    175};
dby5 = {0, 0, 0, 0, 92, 55, 92, 55, 107, 107, 107, 107, 143, 50, 143, 
   50};
dby6 = {0, 0, 0, 0, 22, 78, 22, 78, 88, 88, 88, 88, 98, 138, 98, 138};

n = 5;

step = 1;



d3 = List[dby1, dby2, dby3];
d4 = List[dby4, dby5, dby6];



sets3 = Drop[Subsets[{dbx1, dbx2, dbx3}, 3], 1];
sets4 = Drop[Subsets[{dbx4, dbx5, dbx6}, 3], 1];

dy = d4;

(*ricerca di plaintext adeguati 1*)


(*I passaggi sono molto simili alla fase precedente l'unica \
differenza e che avvendo scelto questo tipo di caratteristiche le \
S-Box attive si trovano nel penultimo Round e non nell'ultimo come \
prima, quindi i nostri testi cifrati devo soddisfare proprietà delle \
caratteristiche differenti*)

l1 = GeneratorPlaintext[n, sets4];
c1 = ChipertextPairs[l1, n, step];
s = s2;

caratteristiche1 = Transpose[RightChar[c1, n]];


cp1 = Flatten[
   List[ScelteCoppie[caratteristiche1, 1, dy, n], 
    ScelteCoppie[caratteristiche1, 2, dy, n], 
    ScelteCoppie[caratteristiche1, 3, dy, n]], 1];


rightcupple1 = 
  First[Flatten[Map[Position[cp1, # ] &, Range[6, 16]], 1] /. {} -> 
     Nothing, {}];






(*keyfirst è la porzione di chiave trovata nella fase 1*)

keyfirst = {40, 41, 42, 43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
position1 = 
 If[rightcupple1[[1]] < n + 1, 1, 
  If[n < rightcupple1[[1]] < 2 n + 1, 2, 3
   ]]






a1 = caratteristiche1[[position1, 
   rightcupple1[[1]] - n (position1 - 1), rightcupple1[[2]]]];










Trovaelemento[x_, dy_, position_, keyfirst_] := 
 If[Count[
    BitXor[BitXor @@ (Map[BitXor[keyfirst, x[[#]]] &, Range[1, 2]]), 
     dy[[position]]], 0] > 7, 1, 0]














(* utilizziamo di nuovo l'algoritmo trova 2 coppie ma questa volta le \
diemo una variabile aggiuntiva "TrovaElemento" che verifica una \
condizione supplementare )




TrovaElemento[n_, step_, sets_, dy_, keyfirst_] := 
 Module[{l, c, caratteristiche, cp, rightcupple, position, a1, 
   elemento},
  l = GeneratorPlaintext[n, sets];
  c = ChipertextPairs[l, n, step];
  caratteristiche = Transpose[RightChar[c, n]];
  cp = Flatten[
    List[ScelteCoppie[caratteristiche, 1, dy, n], 
     ScelteCoppie[caratteristiche, 2, dy, n], 
     ScelteCoppie[caratteristiche, 3, dy, n]], 1];
  rightcupple = 
   First[Flatten[Map[Position[cp, # ] &, Range[6, 16]], 1] /. {} -> 
      Nothing, {}];
  If[rightcupple === {},
   {},
   position = 
    If[rightcupple[[1]] < n + 1, 1, 
     If[n < rightcupple[[1]] < 2 n + 1, 2, 3
      ]];
   a1 = caratteristiche[[position, 
     rightcupple[[1]] - n (position - 1), rightcupple[[2]]]];
   elemento = Trovaelemento[a1, dy, position, keyfirst];
   If[elemento === 1,
    List[a1, position],
    {}
    ]
   ]
  ]










Trova2Coppie1[n_, step_, sets_, dy_, keyfirst_, a_] := 
 Module[{h, c, i},
  i = 1;
  h = {{, }, {, }};
  While[h[[1, 2]] === Null,
   c = TrovaElemento[n, step, sets, dy, keyfirst];
   If[c === {}, h,
    If[h[[1, 1]] === Null,
     Print["trovata prima coppia" ];
     h[[1, 1]] = c[[1]];
     h[[2, 1]] = c[[2]],
     h[[1, 2]] = c[[1]];
     h[[2, 2]] = c[[2]];
     Print["trovata seconda coppia"]
     ]
    ];
   Print[ciclofinito_, a _, i];
   i++;
   ];
  h
  ]





















(*ricerca di plaintext adeguati 2, simile all'algoritmo della fase \
uno ma aggiunge la porzione di chiave trovata precedentemente*)



CharKey1[x_, y_, step_] := 
 Table[BitXor[
   SubByteInverse[
    AddCostant[ShiftRowsInverse[x[[a, b]]], 4*step - 1]], 
   SubByteInverse[
    AddCostant[ShiftRowsInverse[y[[a, b]]], 4*step - 1]]], {a, 1, 
   256}, {b, 1, 256}]


testers[x_, DIFF_, step_, keyfirst_] := 
  CharKey1[
   DIFF[MixColumsInverse[
     SubByteInverse[
      BitXor[AddCostant[ShiftRowsInverse[x[[1]]], 4*step], 
       keyfirst]]]], 
   DIFF[MixColumsInverse[
     SubByteInverse[
      BitXor[AddCostant[ShiftRowsInverse[x[[2]]], 4*step], 
       keyfirst]]]], step];

TrovaChar[x_, DIFF_, step_, keyfirst_] := 
 Map[testers[x[[#]], DIFF, step, keyfirst] &, Range[1, 2]]






charposition1 = 
  List[TrovaChar[coppie1[[1]], DIFF2, step, keyfirst], coppie1[[2]]];







chiavifinale1 = 
  Flatten[Cases[
      Tally[Flatten[
        ChiaviComuni[charposition1[[1]], s2, charposition1[[2]]], 
        1]], {_, 2}], 1][[1]] - 1;



 





(*Lalgoritmo che trova i secondi 32 bit chiave equivalente conoscendo \
i primi 32*)




Trova32BitChiave1[n_, step_, keyfirst_] := 
 Module[{coppie1, coppie2, charposition1, charposition2, 
   chiavifinale1, chiavifinale2, a, b},
  coppie1 = Trova2Coppie1[n, step, sets3, d3, keyfirst, "a"];
  coppie2 = Trova2Coppie1[n, step, sets4, d4, keyfirst, "b"];
  charposition1 = 
   List[TrovaChar[coppie1[[1]], DIFF1, step, keyfirst], 
    coppie1[[2]]];
  charposition2 = 
   List[TrovaChar[coppie2[[1]], DIFF2, step, keyfirst], 
    coppie2[[2]]];
  
  chiavifinale1 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition1[[1]], s1, charposition1[[2]]], 
         1]], {_, 2}], 1][[1]] - 1;
  
  chiavifinale2 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition2[[1]], s2, charposition2[[2]]], 
         1]], {_, 2}], 1][[1]] - 1;
  List[chiavifinale2[[1]], chiavifinale1[[1]], chiavifinale2[[2]], 
   chiavifinale1[[2]]]
  
  ]












(*Trova i 64 bit chiave equivalente *)



Trova64bitchiave[n_, step_] := Module[{keyfirst, a, keysecond},
  
  keyfirst = Trova32BitChiave [n, step];
  a = Flatten[List[keyfirst, Table[0, 12]]];
  keysecond = Trova32BitChiave1[n, step, a];
  List[keysecond, keyfirst]
  ]
















(*fase 3 conveniente solo per step inferiori a 4  *)










(*Il procedimento è simile a quello delle fasi precedenti con la \
differenza che le S-Box attive si trovano nel secondo Round di ogni \
step e dobbiamo usare le informazioni delle porzioni di chiavi già \
trovate *)
BitXor[FieldTimes[3, 55], FieldTimes[4, 92]]


dcx1 = {0, 0, 0, 0, 20, 178, 20, 178, 254, 254, 254, 254, 185, 51, 
   185, 51};
dcx2 = {0, 0, 0, 0, 255, 225, 255, 225, 169, 169, 169, 169, 89, 145, 
   89, 145};
dcx3 = {0, 0, 0, 0, 80, 254, 80, 254, 213, 213, 213, 213, 210, 204, 
   210, 204};
dcx4 = {0, 0, 0, 0, 178, 20, 178, 20, 254, 254, 254, 254, 51, 185, 51,
    185};
dcx5 = {0, 0, 0, 0, 225, 225, 225, 225, 169, 169, 169, 169, 145, 89, 
   145, 89};
dcx6 = {0, 0, 0, 0, 25, 254, 25, 254, 80, 213, 80, 213, 213, 204, 213,
    204};



dcy1 = {0, 0, 0, 0, 164, 178, 164, 178, 88, 0, 88, 0, 175, 205, 175, 
   205};
dcy2 = {0, 0, 0, 0, 138, 225, 138, 225, 183, 0, 183, 0, 50, 56, 50, 
   56};
dcy3 = {0, 0, 0, 0, 166, 254, 166, 254, 123, 0, 123, 0, 138, 25, 138, 
   25};
dcy4 = {0, 0, 0, 0, 178, 164, 178, 164, 0, 88, 0, 88, 205, 175, 205, 
   175};
dcy5 = {0, 0, 0, 0, 225, 138, 225, 138, 0, 183, 0, 183, 56, 50, 56, 
   50};
dcy6 = {0, 0, 0, 0, 254, 166, 254, 166, 0, 123, 0, 123, 25, 138, 25, 
   138};




sets5 = Drop[Subsets[{dcx1, dcx2, dcx3}, 3], 1];
sets6 = Drop[Subsets[{dcx4, dcx5, dcx6}, 3], 1];
d5 = List[dcy1, dcy2, dcy3];
d6 = List[dcy4, dcy5, dcy6];
n = 5;
step = 1;
dy = d3;





l2 = GeneratorPlaintext[n, sets5];

(*algoritmo per verificare la differenza step 3*)
CoppiaChiper1[l_, x_, y_, step_, keyfirst_] := 
 Map[MixColumsInverse[
    SubByteInverse[
     ShiftRowsInverse[
      BitXor[AddCostant[
        MixColumsInverse[Zorro[l[[x, y, #]], step][[1]]], 4*step], 
       keyfirst]]]] &, Range[1, 2]]
StrutturaChiper1[x_, n_, step_, keyfirst_] := 
 Map[CoppiaChiper1[x, n, #, step, keyfirst] &, Range[1, 12]]
ChipertextPairs1[x_, n_, step_, keyfirst_] := 
 Map[StrutturaChiper1[x, #, step, keyfirst] &, Range[1, n]]



c2 = ChipertextPairs1[l2, n, step, keyfirst];
caratteristiche2 = Transpose[RightChar[c2, n]];

cp2 = Flatten[
  List[ScelteCoppie[caratteristiche2, 1, dy, n], 
   ScelteCoppie[caratteristiche2, 2, dy, n], 
   ScelteCoppie[caratteristiche2, 3, dy, n]], 1]
{{8, 8, 8, 8}, {8, 8, 12, 8}, {8, 8, 8, 8}, {8, 8, 8, 8}, {8, 8, 12, 
  8}, {8, 8, 12, 8}, {8, 8, 8, 8}, {8, 8, 8, 8}, {8, 8, 8, 8}, {8, 8, 
  8, 8}, {8, 12, 8, 8}, {8, 8, 8, 8}, {8, 8, 8, 8}, {8, 8, 8, 8}, {8, 
  8, 8, 8}}


rightcupple2 = 
 First[Flatten[Map[Position[cp2, # ] &, Range[6, 16]], 1] /. {} -> 
    Nothing, {}]
{1, 1}


position2 = 
 If[rightcupple2[[1]] < n + 1, 1, 
  If[n < rightcupple2[[1]] < 2 n + 1, 2, 3
   ]]

a2 = caratteristiche1[[position2, 
  rightcupple2[[1]] - n (position2 - 1), rightcupple2[[2]]]]
{{122, 178, 217, 213, 65, 154, 57, 221, 178, 68, 178, 192, 130, 183, 
  68, 157}, {6, 224, 177, 112, 76, 148, 33, 78, 53, 101, 119, 105, 
  176, 231, 11, 51}}




keysecond = {133, 142, 155, 144, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};




(*implemento algoritmo per attacco massimo 2 step di Zorro*)


TrovaElemento[n_, step_, sets_, dy_, keyfirst_, keysecond_] := 
 Module[{l, c, caratteristiche, cp, rightcupple, position, a1, 
   elemento},
  l = GeneratorPlaintext[n, sets];
  c = ChipertextPairs1[l, n, step, keyfirst];
  caratteristiche = Transpose[RightChar[c, n]];
  cp = Flatten[
    List[ScelteCoppie[caratteristiche, 1, dy, n], 
     ScelteCoppie[caratteristiche, 2, dy, n], 
     ScelteCoppie[caratteristiche, 3, dy, n]], 1];
  rightcupple = 
   First[Flatten[Map[Position[cp, # ] &, Range[6, 16]], 1] /. {} -> 
      Nothing, {}];
  If[rightcupple === {},
   {},
   position = 
    If[rightcupple[[1]] < n + 1, 1, 
     If[n < rightcupple[[1]] < 2 n + 1, 2, 3
      ]];
   a1 = caratteristiche[[position, 
     rightcupple[[1]] - n (position - 1), rightcupple[[2]]]];
   elemento = Trovaelemento[a1, dy, position, keysecond];
   If[elemento === 1,
    List[a1, position],
    {}
    ]
   ]
  ]





Trova2Coppie1[n_, step_, sets_, dy_, keyfirst_, keysecond_, a_] := 
 Module[{h, c, i},
  i = 1;
  h = {{, }, {, }};
  While[h[[1, 2]] === Null,
   c = TrovaElemento[n, step, sets, dy, keyfirst, keysecond];
   If[c === {}, h,
    If[h[[1, 1]] === Null,
     Print["trovata prima coppia" ];
     h[[1, 1]] = c[[1]];
     h[[2, 1]] = c[[2]],
     h[[1, 2]] = c[[1]];
     h[[2, 2]] = c[[2]];
     Print["trovata seconda coppia"]
     ]
    ];
   Print[ciclofinito_, a _, i];
   i++;
   ];
  h
  ]





coppie2 = Trova2Coppie1[5, 1, sets5, d3, keyfirst, keysecond, "a"]



(*Raundattack serve per decifrare parzialmente un round aggiungendo ( \
attraverso lo Xor) la porzione di chaive già trovata*)

Raundattack[x_, key_, round_] := 
 MixColumsInverse[
  SubByteInverse[BitXor[AddCostant[ShiftRowsInverse[x], round], key]]]







CharKey2[x_, y_, step_] := 
 Table[BitXor[
   SubByteInverse[
    AddCostant[ShiftRowsInverse[x[[a, b]]], 4*step - 2]], 
   SubByteInverse[
    AddCostant[ShiftRowsInverse[y[[a, b]]], 4*step - 2]]], {a, 1, 
   256}, {b, 1, 256}]


testers2[x_, DIFF_, step_, keysecond_] := 
  CharKey2[DIFF[Raundattack[x[[1]], keysecond, 4*step - 1]], 
   DIFF[Raundattack[x[[2]], keysecond, 4*step - 1]], step];

TrovaChar2[x_, DIFF_, step_, keysecond_] := 
 Map[testers2[x[[#]], DIFF, step, keysecond] &, Range[1, 2]]




charposition2 = 
  List[TrovaChar2[coppie2[[1]], DIFF1, step, keysecond], coppie2[[2]]];



chiavifinale2 = 
 Flatten[Cases[
     Tally[Flatten[
       ChiaviComuni[charposition2[[1]], s1, charposition2[[2]]], 
       1]], {_, 2}], 1][[1]] - 1











(*algoritmo che trova 96 bit chiave, conviene solo per step inferiori \
a 4 *)





Trova96BitChiave[n_, step_] := 
 Module[{key, coppie1, coppie2, charposition1, charposition2, 
   chiavifinale1, chiavifinale2, key1, key2}, 
  key = Trova64bitchiave[n, step]; Print[key]; 
  key1 = Flatten[{key[[2]], Table[0, 12]}]; 
  key2 = Flatten[{key[[1]], Table[0, 12]}]; 
  coppie1 = Trova2Coppie1[n, step, sets5, d3, key1, key2, "a"]; 
  coppie2 = Trova2Coppie1[n, step, sets6, d4, key1, key2, "b"]; 
  charposition1 = {TrovaChar2[coppie1[[1]], DIFF1, step, key2], 
    coppie1[[2]]}; 
  charposition2 = {TrovaChar2[coppie2[[1]], DIFF2, step, key2], 
    coppie2[[2]]}; 
  chiavifinale1 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition1[[1]], s1, charposition1[[2]]], 
         1]], {_, 2}], 1][[1]] - 1; 
  chiavifinale2 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition2[[1]], s2, charposition2[[2]]], 
         1]], {_, 2}], 1][[1]] - 1; 
  Partition[
   Flatten[{{chiavifinale2[[1]], chiavifinale1[[1]], 
      chiavifinale2[[2]], chiavifinale1[[2]]}, Flatten[key, 1]}, 1], 4]
  ]













keythird = Flatten[List[{249, 155, 69, 39}, Table[0, 12]]]



(*fase 4 conviene solo per step inferiori a 3 *)




(* Ricerca di tutte le equazioni di chiavi equivalenti per poter \
estrarre la chiave finale, conveniente solo fino allo step 3 *)




sets7 = Drop[Subsets[s1, 3], 1];
sets8 = Drop[Subsets[s2, 3], 1];
dy = d3;

l3 = GeneratorPlaintext[n, sets7];


(*algoritmo per verificare la differenza step 3*)

CoppiaChiper2[l_, x_, y_, step_, keyfirst_, keysecond_] := 
 Map[Raundattack[
    Raundattack[MixColumsInverse[Zorro[l[[x, y, #]], step][[1]]], 
     keyfirst, 4*step], keysecond, 4*step - 1] &, Range[1, 2]]
StrutturaChiper2[x_, n_, step_, keyfirst_, keysecond_] := 
 Map[CoppiaChiper2[x, n, #, step, keyfirst, keysecond] &, Range[1, 12]]
ChipertextPairs2[x_, n_, step_, keyfirst_, keysecond_] := 
 Map[StrutturaChiper2[x, #, step, keyfirst, keysecond] &, Range[1, n]]



c3 = ChipertextPairs2[l3, n, step, keyfirst, keysecond];
caratteristiche3 = Transpose[RightChar[c3, n]];




cp3 = Flatten[
   List[ScelteCoppie[caratteristiche3, 1, dy, n], 
    ScelteCoppie[caratteristiche3, 2, dy, n], 
    ScelteCoppie[caratteristiche3, 3, dy, n]], 1];











TrovaElemento[n_, step_, sets_, dy_, keyfirst_, keysecond_, 
  keythird_] := 
 Module[{l, c, caratteristiche, cp, rightcupple, position, a1, 
   elemento},
  l = GeneratorPlaintext[n, sets];
  c = ChipertextPairs2[l, n, step, keyfirst, keysecond];
  caratteristiche = Transpose[RightChar[c, n]];
  cp = Flatten[
    List[ScelteCoppie[caratteristiche, 1, dy, n], 
     ScelteCoppie[caratteristiche, 2, dy, n], 
     ScelteCoppie[caratteristiche, 3, dy, n]], 1];
  rightcupple = 
   First[Flatten[Map[Position[cp, # ] &, Range[6, 16]], 1] /. {} -> 
      Nothing, {}];
  If[rightcupple === {},
   {},
   position = 
    If[rightcupple[[1]] < n + 1, 1, 
     If[n < rightcupple[[1]] < 2 n + 1, 2, 3
      ]];
   a1 = caratteristiche[[position, 
     rightcupple[[1]] - n (position - 1), rightcupple[[2]]]];
   elemento = Trovaelemento[a1, dy, position, keythird];
   If[elemento === 1,
    List[a1, position],
    {}
    ]
   ]
  ]






Trova2Coppie2[n_, step_, sets_, dy_, keyfirst_, keysecond_, keythird_,
   a_] := Module[{h, c, i},
  i = 1;
  h = {{, }, {, }};
  While[h[[1, 2]] === Null,
   c = TrovaElemento[n, step, sets, dy, keyfirst, keysecond, keythird];
   If[c === {}, h,
    If[h[[1, 1]] === Null,
     Print["trovata prima coppia" ];
     h[[1, 1]] = c[[1]];
     h[[2, 1]] = c[[2]],
     h[[1, 2]] = c[[1]];
     h[[2, 2]] = c[[2]];
     Print["trovata seconda coppia"]
     ]
    ];
   Print[ciclofinito_, a _, i];
   i++;
   ];
  h
  ]




coppie3 = 
  Trova2Coppie2[5, 1, sets7, dy, keyfirst, keysecond, keythird, "a"];





CharKey3[x_, y_, step_] := 
 Table[BitXor[
   SubByteInverse[
    AddCostant[ShiftRowsInverse[x[[a, b]]], 4*step - 3]], 
   SubByteInverse[
    AddCostant[ShiftRowsInverse[y[[a, b]]], 4*step - 3]]], {a, 1, 
   256}, {b, 1, 256}]


testers3[x_, DIFF_, step_, key_] := 
  CharKey3[DIFF[Raundattack[x[[1]], key, 4*step - 2]], 
   DIFF[Raundattack[x[[2]], key, 4*step - 2]], step];

TrovaChar3[x_, DIFF_, step_, key_] := 
 Map[testers3[x[[#]], DIFF, step, key] &, Range[1, 2]]


charposition3 = 
  List[TrovaChar3[coppie3[[1]], DIFF1, step, keythird], coppie3[[2]]];



chiavifinale3 = 
 Flatten[Cases[
     Tally[Flatten[
       ChiaviComuni[charposition3[[1]], s1, charposition3[[2]]], 
       1]], {_, 2}], 1][[1]] - 1







(*algoritmo che trova 4 chiavi equivalenti che messe a sistema \
trovano tutti i 128 bit di chiave, conviene solo per step inferiori a \
3 *)


Trova128BitChiave[n_, step_] := 
 Module[{key, coppie1, coppie2, charposition1, charposition2, 
   chiavifinale1, chiavifinale2, key1, key2, key3},
  key = Trova96BitChiave[n, step]; Print[key]; 
  key1 = Flatten[{key[[3]], Table[0, 12]}]; 
  key2 = Flatten[{key[[2]], Table[0, 12]}];
  key3 = Flatten[{key[[1]], Table[0, 12]}];
  coppie1 = Trova2Coppie2[n, step, sets7, d3, key1, key2, key3, "a"]; 
  coppie2 = Trova2Coppie2[n, step, sets8, d4, key1, key2, key3, "b"]; 
  charposition1 = {TrovaChar3[coppie1[[1]], DIFF1, step, key3], 
    coppie1[[2]]}; 
  charposition2 = {TrovaChar3[coppie2[[1]], DIFF2, step, key3], 
    coppie2[[2]]}; 
  chiavifinale1 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition1[[1]], s1, charposition1[[2]]], 
         1]], {_, 2}], 1][[1]] - 1; 
  chiavifinale2 = 
   Flatten[Cases[
       Tally[Flatten[
         ChiaviComuni[charposition2[[1]], s2, charposition2[[2]]], 
         1]], {_, 2}], 1][[1]] - 1; 
  Partition[
   Flatten[{{chiavifinale2[[1]], chiavifinale1[[1]], 
      chiavifinale2[[2]], chiavifinale1[[2]]}, Flatten[key, 1]}, 1], 4]
  ]




solution128bit = Trova128BitChiave[5, 2]







(*RandomInteger[255,16]*)



key = {46, 238, 16, 212, 220, 23, 204, 239, 46, 151, 110, 23, 6, 25, 
   160, 177};







(*Non sono riuscito a trovare un metodo pratico per implementare la \
risoluzione di sistemi (con coefficienti su campi finiti ) lineari \
multipli per estrarre la chiave avendo a disposizione tutti i valori \
interessati. Anche se FiniteField sembra disponibile su matematica \
non me lo riconosce. Di seguito ho abbozzato una possibile \
implementazione con l'uso di Finite Field. *)


(*
key4=Flatten[List[solution128bit[[1]],Table[Symbol["kd"<>ToString[i]],\
{i,5,16}]]];
Key3=Flatten[List[solution128bit[[2]],Table[Symbol["kc"<>ToString[i]],\
{i,5,16}]]];
key2=Flatten[List[solution128bit[[3]],Table[Symbol["kb"<>ToString[i]],\
{i,5,16}]]];
key1=Flatten[List[solution128bit[[4]],Table[Symbol["ka"<>ToString[i]],\
{i,5,16}]]];
key0=Table[Symbol["k0"<>ToString[i]],{i,1,16}];


F=FiniteField[2,f] 

ReduceEq[x_,y_]:=Reduce[

F[2]F[x[[1]]]+F[3]x[[5]]+F[1]x[[9]]+F[1]x[[13]]==0 &&
F[2]F[x[[2]]+F[3]x[[6]]+F[1]x[[10]]+F[1]x[[14]]==0 &&... ]


in totale sono 16 eq e rappresenta MixColums[x]=ShiftRowsInverse[y]
ricordarsi che gli elementi della seconda e 4 riga di y sono sogette \
alla shift rows inverse quindi sono traslate, ad esempio l'eq 5 sarà \
...=y[[6]] e che la porzione di chiave nella prima riga è nulla



ReduceEq1[x_,y_]:=Reduce[

F[2]F[x[[1]]]+F[3]x[[5]]+F[1]x[[9]]+F[1]x[[13]]==F[y[[1]]] &&
F[2]F[x[[2]]+F[3]x[[6]]+F[1]x[[10]]+F[1]x[[14]]==F[y[[2]]] &&...]

in totale sono 16 eq e rappresenta MixColums[x]=y

la risoluzione del sistema che mette in relazione le variabili di \
keyfirst con la chiave key0 da trovare 


Solve[ReduceEq[key4,key3] && ReduceEq[key3,key2] && \
ReduceEq[key2,key1,] && ReduceEq1[key1,key0]
]
Ci dovrebbe dare come soluzione i 128 bit di chiave cercate
*)









(*
MixColumsInverse[key];
x1a=={0,0,0,0,60,61,62,63,32,33,34,35,52,53,54,55};
x2=MixColumsInverse[ShiftRowsInverse[x1]]
x2a={0,0,0,0,94,80,78,64,247,255,239,231,4,8,16,28}
{0,0,0,0,94,80,78,64,247,255,239,231,4,8,16,28}
x3=MixColumsInverse[ShiftRowsInverse[x2a]]
{249,155,69,39,214,130,58,110,67,53,223,169,203,133,27,85}
x3a={0,0,0,0,214,130,58,110,67,53,223,169,203,133,27,85}
{0,0,0,0,214,130,58,110,67,53,223,169,203,133,27,85}
x4=MixColumsInverse[ShiftRowsInverse[x3a]]
{2,76,80,30,25,88,50,115,181,45,32,184,154,93,214,17}
*)


