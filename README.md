# speck-cipher
Implementation of the Speck cipher in Wolfram Language.

Speck supports a variety of block and key sizes, a block is always two words, but the words may be 16, 24, 32, 48 or 64 bits in size. The corresponding key is 2, 3 or 4 words. The round function consists of two rotations, adding the right word to the left word, xoring the key into the left word, then xoring the left word into the right word.
The number of rounds depends on the parameters selected (view the SetRoundsNumber function for further information).

The key schedule uses the same round function as the main block cipher.
