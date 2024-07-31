(* ::Package:: *)

Get["C:\\Users\\tizia\\Desktop\\Progetto\\LinearizationRround.wl"];

Print["load LinearizationAttack"];


(*Esempio 2*)
plaintext2={1,0,0,1,0,1};
key2={1,1,1,1,0,0};
r1=4;
KMatrix2={{1,0,1,1,0,0},{0,1,0,1,1,0},{1,0,1,0,1,1},{1,1,0,1,0,0},{1,0,1,0,1,0},{0,1,1,0,1,1}};
LMatrix2={{1,0,0,1,1,0},{1,1,0,0,1,0},{1,0,1,0,1,0},{1,1,0,1,0,0},{1,0,0,1,0,0},{0,1,0,0,1,1}};

RoundConstants2={0,1,0,1,0,1};
ciphertext2=LowMCEncrypt[plaintext2,key2,KMatrix2,LMatrix2,RoundConstants2,r1];

Print["La chiave originale \[EGrave]: ",key2];
ChiaveTrovata2=Attack[KMatrix2,LMatrix2,RoundConstants2,plaintext2,r1,ciphertext2];



