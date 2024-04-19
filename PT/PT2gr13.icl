module PT2gr13

import StdEnv

/* Your NAME and NEPTUN code :  */

/* 1. Write a function to count the number of zero digits of a number. */



zeroDigits :: Int -> Int
zeroDigits x = (length (filter ((==) 0) (split x)))


split :: Int -> [Int]
split x
| x<10 = [x]
= [x rem 10] ++ split (x/10)

//Start = split 456

//Start = zeroDigits 456 // 0

//Start = zeroDigits 40201000 // 5

//Start = zeroDigits 7 // 0

//Start = zeroDigits 77777 // 0

//Start = zeroDigits 100000 // 5

//Start = zeroDigits 0 // 1

//Start = zeroDigits 123405678 //1
