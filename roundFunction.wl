(* Rotate the number n to the left of p positions, s is the total bit size of the number *)
ClearAll[RotateBitToTheLeft];
RotateBitToTheLeft[n_, p_, s_] := BitOr[BitAnd[BitShiftLeft[n,p], 2^s - 1], BitShiftRight[n, s - p] ]

(* Rotate the number n to the right of p positions, s is the total bit size of the number*)
ClearAll[RotateBitToTheRight];
RotateBitToTheRight[n_, p_, s_] := BitOr[BitShiftRight[n, p],BitAnd[ BitShiftLeft[n, s - p], 2^s - 1] ]

(* RoundFunction, the core of Speck cipher *)
ClearAll[RoundFunction];
RoundFunction[pt1_, pt2_, key_] := Module[{listOutput},
    i1 = Interpreter["HexInteger"][pt1];
    i2 = Interpreter["HexInteger"][pt2];
    lenPt1 = StringLength[pt1]*4;
    lenPt2 = StringLength[pt2]*4;

    rotPt1 = RotateBitToTheRight[i1, 8, lenPt1];
    sumInt = Mod[rotPt1 +i2, 2^lenPt1];
    rotPt2 = RotateBitToTheLeft[i2, 3, lenPt2];
    output1 = IntegerString[BitXor[sumInt, FromDigits[key, 16] ], 16, lenPt1/4];
    output2 = IntegerString[BitXor[FromDigits[output1, 16], rotPt2], 16,lenPt1/4];
    listOutput = List[output1, output2];

    Return[listOutput];
];

(* Applies the same operations as RoundFunction but reversed, it's used in order to decrypt the ciphertext *)
ClearAll[ReversedRoundFunction];
ReversedRoundFunction[ct1_, ct2_, key_] := Module[{output},
    i1 = Interpreter["HexInteger"][ct1];
    i2 = Interpreter["HexInteger"][ct2];
    lenCt1 = StringLength[ct1]*4;
    lenCt2 = StringLength[ct2]*4;

    y = BitXor[i1, i2];
    pt2 = RotateBitToTheRight[y, 3, lenCt1];
    x = BitXor[i1,FromDigits[key, 16] ];
    modDiff = Mod[x - pt2, 2^lenCt1];
    pt1 = RotateBitToTheLeft[modDiff, 8, lenCt1];
    output = List[pt1, pt2];
    hexOutput = Map[IntegerString[#, 16, lenCt1/4]&, output];

    Return[hexOutput];
];
