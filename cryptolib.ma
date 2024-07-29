AlphaTest[x_] := 0 <= x <= 25;



TextCode[text_] := Select[
   					ToCharacterCode[
     						ToUpperCase[
           StringReplace[text, {"è" -> "e", "é" -> "e", "ò" -> "o", "ì" -> "i",
  "í" -> "i", "ù" -> "u"}]
                          ]] - 65, AlphaTest];
FromCode[textcode_] := FromCharacterCode[textcode + 65];

CoincidenceIndex[testo_] :=
 If[
  StringQ[testo],
  (*THEN*)
  CoincidenceIndex[TextCode[testo]],
  (*ELSE*)
  Module[{n, freqs},
   (
    n = Length[testo];
    freqs = Map[Count[testo, #] &, Range[0, 25]];
    N[Plus @@ (freqs (freqs - 1)/(n (n - 1)))]
    )]
  ];


Kasiski[text_,a_,b_] := Module[{},
    If[StringQ[text],
    Kasiski[TextCode@text,a,b],
     (
   distances = Table[ngrams = Partition[text, bsize, 1];
     
     (*selezioni gli n-grammi con almeno 2 occorrenze *)
     
     repetitions = Select[SortBy[Tally[ngrams], Last], #[[2]] > 1 &];
     reps = Map[First, repetitions];
     
     Print[Map[FromCode,reps]];
     
     (* per ogni n-gramma trovo le posizioni *)
     positions =
      Map[Map[#[[1]] &, Position[ngrams, #]] &, reps];
     
     Map[Drop[#, 1] - Drop[#, -1] &, positions], {bsize, a, b}];
   Print[Map[FactorInteger, Flatten[distances]]];
   GCD @@ Flatten[distances]
   )]];

Distribution[testo_] :=  
 If[
  StringQ[testo],
  (*THEN*)
    Distribution[TextCode[testo]],
  (*ELSE*)
    Map[Count[testo, #]/Length[testo] &, Range[0, 25]]];

alphabet = Map[FromCode, Range[0, 25]];
DistributionA[testo_]:=Sort@Transpose@{N@Distribution[testo], alphabet};


MutualInformation[testo1_, testo2_] :=  Plus @@ (N@Distribution[testo1] N@Distribution[testo2]);

