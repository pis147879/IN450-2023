ClearAll[RandomHexString, createPairs, FromByteToHex, FromHexToByte]

(*Genera una stringa random di caratteri esadecimali di lunghezza a \
scelta*)
RandomHexString[length_] := 
 StringJoin[
  RandomChoice[{"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a",
     "b", "c", "d", "e", "f"}, length]]

(*Funzione che crea una lista di coppie di lettere a partire da una \
stringa*)
createPairs[str_String] := 
 Table[StringTake[str, {i, i + 1}], {i, 1, StringLength[str] - 1, 2}]

(*Trasforma una stringa decimale in una lista di Byte*)
FromHexToByte[hex_] := Module[{cp},
  If[Mod[Length[Characters[hex]], 2] == 0 && 
    StringMatchQ[hex, HexadecimalCharacter .. ], ,
   Return[
    "Input must be a string of hexadecimal characters and length must \
be an even number"]];
  cp = createPairs[hex];
  Map[FromDigits[#, 16] &, cp]]

(*Trasforma una lista di numeri interi compresi tra 0 e 255 \
interpretati come elementi di GF[2^8] nella corrisponente stringa \
esadecimale*)
FromByteToHex[byte_?ListQ] := StringJoin[IntegerString[byte, 16, 2]]
