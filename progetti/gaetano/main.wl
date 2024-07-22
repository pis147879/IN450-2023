<< finitefields.wl
<< utility.wl
<< halfloop.wl

(*Test cifratura e decifratura Halfloop24*)
plaintext = "010203";
tweak = "543bd88000017550";
key = "2b7e151628aed2a6abf7158809cf4f3c";
DecHL24[EncHL24[plaintext, tweak, key], tweak, key] == plaintext


(*Test proposto dall'articolo sul primo round della cifrante*)
state = FromHexToByte["010203"];
FromByteToHex[state]
state = AddRoundKey[rk[[1]], state];
FromByteToHex[state]
state = SubByte[state];
FromByteToHex[state]
state = RotateRows[state];
FromByteToHex[state]
state = MixColumns[state];
FromByteToHex[state]


<< boomerangattack.wl

