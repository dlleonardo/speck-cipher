
SetRoundsNumber[block_, key_] := Module[{rounds},
    joinedBlock = StringJoin[block];
    joinedKey = StringJoin[key];
    rounds = 22;

    If[StringLength[joinedBlock]*4== 48 && StringLength[joinedKey]*4== 96,rounds = 23];
    If[StringLength[joinedBlock]*4== 64 && StringLength[joinedKey]*4==96, rounds = 26];
    If[StringLength[joinedBlock]*4== 64 && StringLength[joinedKey]*4==128, rounds = 27];
    If[StringLength[joinedBlock]*4== 96 && StringLength[joinedKey]*4==96, rounds = 28];
    If[StringLength[joinedBlock]*4== 96 && StringLength[joinedKey]*4==144, rounds = 29];
    If[StringLength[joinedBlock]*4== 128 && StringLength[joinedKey]*4==128, rounds = 32];
    If[StringLength[joinedBlock]*4== 128 && StringLength[joinedKey]*4==192, rounds = 33];
    If[StringLength[joinedBlock]*4== 128 && StringLength[joinedKey]*4==256, rounds = 34];

    Return[rounds];
]

(* Test Vectors *)
(* Speck 32/64 *)
block = List["6574","694c"];
key = List["1918","1110","0908","0100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 32/64: ", rounds];

(* Speck 48/72 *)
block2 = List["20796c", "6c6172"];
key2 = List["121110","0a0908", "020100"];
rounds = SetRoundsNumber[block2, key2];
Print["Rounds number of Speck 48/72: ", rounds];

(* Speck 48/96 *)
block3 = List["6d2073","696874"];
key3 = List["1a1918","121110","0a0908","020100"];
rounds = SetRoundsNumber[block3, key3];
Print["Rounds number of Speck 48/96: ", rounds];

(* Speck 64/96 *)
block4 = List["74614620","736e6165"];
key4 = List["13121110","0b0a0908","03020100"];
rounds = SetRoundsNumber[block4, key4];
Print["Rounds number of Speck 64/96: ", rounds];

(* Speck 64/128 *)
block5 = List["3b726574","7475432d"];
key5 = List["1b1a1918","13121110","0b0a0908","03020100"];
rounds = SetRoundsNumber[block5, key5];
Print["Rounds number of Speck 64/128: ", rounds];

(* Speck 96/96 *)
block6 = List["65776f68202c","656761737520"];
key6 = List["0d0c0b0a0908","050403020100"];
rounds = SetRoundsNumber[block6, key6];
Print["Rounds number of Speck 96/96: ", rounds];

(* Speck 96/144 *)
block7 = List["656d6974206e","69202c726576"];
key7 = List["151413121110","0d0c0b0a0908","050403020100"];
rounds = SetRoundsNumber[block7, key7];
Print["Rounds number of Speck 96/144: ", rounds];

(* Speck 128/128 *)
block8 = List["6c61766975716520","7469206564616d20"];
key8 = List["0f0e0d0c0b0a0908","0706050403020100"];
rounds = SetRoundsNumber[block8, key8];
Print["Rounds number of Speck 128/128: ", rounds];

(* Speck 128/192 *)
block9 = List["7261482066656968","43206f7420746e65"];
key9 = List["1716151413121110","0f0e0d0c0b0a0908","0706050403020100"];
rounds = SetRoundsNumber[block9, key9];
Print["Rounds number of Speck 128/192: ", rounds];

(* Speck 128/256 *)
block10 = List["65736f6874206e49","202e72656e6f6f70"];
key10 = List["1f1e1d1c1b1a1918","1716151413121110","0f0e0d0c0b0a0908","0706050403020100"];
rounds = SetRoundsNumber[block10, key10];
Print["Rounds number of Speck 128/256: ", rounds];