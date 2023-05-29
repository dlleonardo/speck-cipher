(* GenerateKeySchedule returns the input List in order to generate the ciphertext of each round *)
GenerateKeySchedule[key_List, rounds_Integer, numKeyWords_Integer] := Module[{outKeySchedule},
    keySchedule = ConstantArray[0, rounds];
    tmp = ConstantArray[0, rounds + numKeyWords - 1];
    wordSize = StringLength[key[[1]]];
    bitSize = wordSize*4;

    Print["wordSize: ", wordSize];
    Print["bitSize: ", bitSize];
    Print["numKeyWords: ", numKeyWords];

    If[numKeyWords == 4, 
        keySchedule[[1]] = FromDigits[key[[4]], 16];
        tmp[[1]] = FromDigits[key[[3]], 16];
        tmp[[2]] = FromDigits[key[[2]], 16];
        tmp[[3]] = FromDigits[key[[1]], 16];
    ];
    If[numKeyWords == 3,
        keySchedule[[1]] = FromDigits[key[[3]], 16];
        tmp[[1]] = FromDigits[key[[2]], 16];
        tmp[[2]] = FromDigits[key[[1]], 16];
    ];
    If[numKeyWords == 2,
        keySchedule[[1]] = FromDigits[key[[2]], 16];
        tmp[[1]] = FromDigits[key[[1]], 16];
    ];

    j = 0;
    Do[
        tmp[[i]] = BitXor[ Mod[ RotateBitToTheRight[tmp[[i]], 8, bitSize] + keySchedule[[i]], 2^bitSize ], j ];
        y = BitXor[ RotateBitToTheLeft[keySchedule[[i]], 3, bitSize], tmp[[i]]];
        tmp[[i + numKeyWords - 1]] = tmp[[i]];
        keySchedule[[i+1]] = y;
        j = j + 1;
        ,
        {i, 1, rounds-1}
    ];
    
    outKeySchedule = Map[IntegerString[#, 16, wordSize]&, keySchedule];

    Return[outKeySchedule];
];