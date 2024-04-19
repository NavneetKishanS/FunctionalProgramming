module HW5
import StdEnv

/*
Please write your name and neptun code here: 
Your submission should not have any errors when running the code.
You'll receive a total of 100 points when you successfully solve both problems,
with 50 points awarded for each.

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.
*/

/*
Problem1:
Write a function intInsertion that takes two integers, 
nunumber1m1 and number2, and returns an integer represents the interleaved digits of the input numbers.

Examples:
intInsertion 123 123 returns 112233 
because the digits of 123 and 123 are interleaved to form 112233.
*/

listToInt :: [Int] -> Int
listToInt [] = 0
listToInt [x:xs] = x * (10^(length xs)) + listToInt xs

split :: Int -> [Int]
split x
| x<10 = [x]
= [x rem 10] ++ split (x/10)

intInsertion :: Int Int -> Int
intInsertion a b = listToInt (flatten (reverse[[x,y] \\ x <- (split a) & y <- (split b)]))

//Start = intInsertion 123 123 // 112233
//Start = intInsertion 123 321 // 132231
//Start = intInsertion 13 13 // 1133
//Start = intInsertion 1 2 // 12
//Start = intInsertion 2 1 // 21

/*
Problem2:
You are given a two-dimensional list of real numbers. 
Each sublist contains real values. 

Your task is to perform the following steps:
1. Calculate the sum of each sublist.
2. Convert the sum into an integer by multiplying it.

Write a function sumAndConvert that takes a two-dimensional list of 
real numbers as input and returns a list of integers obtained 
by performing the above steps.

Example:
sumAndConvert [[2.25, 3.75], [1.33, 4.66, 7.99]]
In this case, the function processes a list containing three sublists. 
- The first sublist [2.25, 3.75] has a sum of 6.0, which is converted to 6 as there is no fractional part.
- The second sublist [1.33, 4.66, 7.99] has a sum of 13.98, which is converted to 1398.
The final output is  [6,1398].
*/

decimalToIntConverter :: Real -> Int
decimalToIntConverter a
| (x rem 1000 == 0) = x/1000
| (x rem 100 == 0) = x/100
| (x rem 10 == 0) = x/10
= x
where x =toInt(a*1000.0)

sumAndConvert :: [[Real]] -> [Int]
sumAndConvert [] = []
sumAndConvert [[]] = []
sumAndConvert ls = filter ((<>) 0) (map decimalToIntConverter (map (sum) ls) )



//Start = sumAndConvert [] // []
//Start = sumAndConvert [[2.25, 3.75], [1.33, 4.66, 7.99]] // [6,1398]
//Start = sumAndConvert [[1.2, 1.3, 1.5], [3.53, 53.42, 53.21]] // [4, 11016]
//Start = sumAndConvert [[], [34.78, 4.98, 8.9, 0.0]] // [4866]
//Start = sumAndConvert [[2.25, 3.75], [1.33, 4.66, 7.99], [0.5]] //[6,1398,5]

