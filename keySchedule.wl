(* New key schedule generation function *)
blockKey = List["1f1e1d1c1b1a1918","1716151413121110","0f0e0d0c0b0a0908","0706050403020100"];
rounds = 22;
m=4;
keySchedule = ConstantArray[0,rounds];
tmp = ConstantArray[0, rounds+3];
keySchedule[[1]] = FromDigits[blockKey[[4]], 16];
tmp[[1]] = FromDigits[blockKey[[3]], 16];
tmp[[2]] = FromDigits[blockKey[[2]], 16];
tmp[[3]] = FromDigits[blockKey[[1]], 16];
Print["keySchedule: ", keySchedule];
Print["tmp: ", tmp];
j = 0;
Do[
 tmp[[i]] = BitXor[ Mod[ RotateBitToTheRight[tmp[[i]], 8, 64] + keySchedule[[i]],2^64], j];
 y = BitXor[RotateBitToTheLeft[keySchedule[[i]], 3, 64],tmp[[i]]];
 tmp[[i + m - 1]] = tmp[[i]];
 keySchedule[[i+1]] = y;
 j = j + 1;
,
{i, 1, rounds-1}
]
mapKeySchedule = Map[IntegerString[#,16,16]&,keySchedule];
Print["hex KeySchedule: ", mapKeySchedule];
Length[mapKeySchedule]