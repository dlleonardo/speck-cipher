(* Rotate the number n to the left of p positions, s is the total bit size of the number *)
ClearAll[RotateBitToTheLeft];
RotateBitToTheLeft[n_, p_, s_] := BitOr[BitAnd[BitShiftLeft[n,p], 2^s - 1], BitShiftRight[n, s - p] ]

(* Rotate the number n to the right of p positions, s is the total bit size of the number*)
ClearAll[RotateBitToTheRight];
RotateBitToTheRight[n_, p_, s_] := BitOr[BitShiftRight[n, p],BitAnd[ BitShiftLeft[n, s - p], 2^s - 1] ]

(* Test on RoundFunction using Speck 96/96 *)
ClearAll[RoundFunction];
RoundFunction[pt1_, pt2_, k_] := Module[{listOutput},
    i1 = FromDigits[Part[pt1, 1], 16];
    i2 = FromDigits[Part[pt2, 1], 16];
    i3 = FromDigits[Part[k, 1], 16];
    
    lenPt1 = 48;
    lenPt2 = 48;
    (* Ruoto di 8 bit a destra il primo input *)
    rotPt1 = RotateBitToTheRight[i1, 8, lenPt1];
    (* Modulo 16 della somma fra il risultato della rotazione precedente e il secondo input *)
    sumInt =rotPt1 + i2;
    sumStr = BitAnd[Mod[sumInt, 48], 2^lenPt1 - 1];
    (*sumStr = BitAnd[BitXor[rotPt1, i2], lenPt1 - 1];*)
    (* Ruoto di 3 bit a sinistra il secondo input *)
    rotPt2 = RotateBitToTheLeft[i2, 3, lenPt2];
    (* Or esclusivo fra il risultato del Modulo 16 precedente e il terzo input *)
    output1 = IntegerString[BitXor[sumStr, i3 ], 16];
    (* Or esclusivo fra il risultato dell'or esclusivo precedente e il risultato della rotazione a sinistra del secondo input *)
    output2 = IntegerString[BitXor[FromDigits[output1, 16], rotPt2 ], 16];
    listOutput = List[output1, output2];
    Return[listOutput];
] 
  
(* Esempio di ciclo Do *)
blockPt = List["65776f68202c","656761737520"];
blockKey = List["0d0c0b0a0908","050403020100"];
rounds = 28;
Do[
    Print["Indice j: ", j];
    hexCounter = List[IntegerString[j, 16] ];

    ret = RoundFunction[Take[blockPt, {1}], Take[blockPt,{2}], Take[blockKey, {2}] ];
    retK = RoundFunction[ Take[blockKey, {1}], Take[blockKey, {2}], Take[hexCounter, {1}] ];
    
    Print["j in hexadecimal: ", hexCounter];
    blockPt = ret;
    blockKey = retK;
    
    Print["blockPt: ", blockPt];
    Print["blockKey: ", blockKey];,
    {j, 0, rounds-1}
]