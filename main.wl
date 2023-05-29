(* Set the rounds number given the plaintext and key blocks *)
ClearAll[SetRoundsNumber];
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
];
(* SpeckEncrypt function, given a plaintext, keySchedule, and rounds number, generates the ciphertext *)
ClearAll[SpeckEncrypt];
SpeckEncrypt[block_, keySchedule_, rounds_] := Module[{ciphertext},
    tmp = block;
    Do[     
        ret = RoundFunction[tmp[[1]], tmp[[2]], keySchedule[[j+1]] ];
        tmp = ret;
        ,
        {j, 0, rounds-1}
    ];
    
    Return[ret]; 
];
(* SpeckDecrypt, given a ciphertext, keySchedule, and rounds number, generates the original plaintext *)
ClearAll[SpeckDecrypt];
SpeckDecrypt[block_, keySchedule_, rounds_] := Module[{plaintext},
    keyScheduleReversed = Reverse[keySchedule];
    tmp = block;
    Do[
        ret = ReversedRoundFunction[tmp[[1]], tmp[[2]], keyScheduleReversed[[i+1]]];
        tmp = ret;
        ,
        {i, 0, rounds-1}
    ];

    Return[ret];
];

(* Test Vectors *)
(* Speck 48/72 *)
Print["Test on Speck 48/72"];
block = List["20796c", "6c6172"];
key = List["121110","0a0908", "020100"];
rounds = SetRoundsNumber[block, key];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 48/96 *)
Print["Test on Speck 48/96"];
block = List["6d2073","696874"];
key = List["1a1918","121110","0a0908","020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 48/96: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 64/96 *)
Print["Test on Speck 64/96"];
block = List["74614620","736e6165"];
key = List["13121110","0b0a0908","03020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 64/96: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 64/128 *)
Print["Test on Speck 64/128"];
block = List["3b726574","7475432d"];
key = List["1b1a1918","13121110","0b0a0908","03020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 64/128: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 96/96 *)
Print["Test on Speck 96/96"];
block = List["65776f68202c","656761737520"];
key = List["0d0c0b0a0908","050403020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 96/96: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 96/144 *)
Print["Test on Speck 96/144"];
block = List["656d6974206e","69202c726576"];
key = List["151413121110","0d0c0b0a0908","050403020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 96/144: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 128/128 *)
Print["Test on Speck 128/128"];
block = List["6c61766975716520","7469206564616d20"];
key = List["0f0e0d0c0b0a0908","0706050403020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 128/128: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 128/192 *)
Print["Test on Speck 128/192"];
block = List["7261482066656968","43206f7420746e65"];
key = List["1716151413121110","0f0e0d0c0b0a0908","0706050403020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 128/192: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];

(* Speck 128/256 *)
Print["Test on Speck 128/256"];
block = List["65736f6874206e49","202e72656e6f6f70"];
key = List["1f1e1d1c1b1a1918","1716151413121110","0f0e0d0c0b0a0908","0706050403020100"];
rounds = SetRoundsNumber[block, key];
Print["Rounds number of Speck 128/256: ", rounds];
keySchedule = GenerateKeySchedule[key, rounds, Length[key] ];
Print["keySchedule: ", keySchedule];

ciphertext = SpeckEncrypt[block, keySchedule, rounds];
Print["ciphertext: ", ciphertext];
plaintext = SpeckDecrypt[ciphertext, keySchedule, rounds];
Print["plaintext: ", plaintext ];