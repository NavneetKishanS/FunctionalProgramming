module midterm4

import StdEnv
/* Instructions
You are given 13 exercises each of 10 points. Min 5 perfect solutions (50 points) is required for passed, max is 100 points, you can choose which ones to solve.

Find on local C: drive the Clean compiler.

Copy the text into a  .icl file, save into your documents folder where you can compile.

Only this browser and compiler should be active during programming.

KEEP this browser open until you submit. MULTIPLE  task  taking IS NOT POSSIBLE.

After you finish coding UPLOAD here the file, JUST .icl file, and PRESS SUBMIT.

After SUBMIT the FILE CAN NOT BE CHANGED, MULTIPLE UPLOADS, MULTIPLE SUBMITS IS NOT POSSIBLE.

 

Your submission should not have any errors when running the code.

It is possible to get partial points for not fully working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add your tests as well.

Don't change the given function signatures, however, you can add as many functions as you wish, just make sure to name them appropriately.

Make sure that you comment all 'Start'-s before submitting the code.

*/



/*---------------------------------------------------------------
-- Functional Programming & mid-term, 2022. Mar. 23.

-- This solution was submitted and prepared by
-- <NAME, NEPTUN> for the mid-term programming assignment of
-- the Functional Programming course.

-- I declare that this solution is my own work.

-- I have not copied or used third-party solutions.

-- I have not passed my solution to my classmates, neither made it public.

-- Students' regulation of Eotvos Lorand University (ELTE Regulations Vol. II. 74/C.)
-- states that as long as a student presents another studentâ€™s work -
-- or at least the significant part of it - as his/her own performance,
-- it will count as a disciplinary fault.

-- The most serious consequence of a disciplinary fault can be dismissal
-- of the student from the University.
*/


//// Disarium number
/*1.
Given a positive integer number, check if the given number
is a Disarium number or not.
A Disarium number is a number defined by the following:
Sum of its digits powered with their respective position
is equal to the original number.
Example: 135 is a Disarium number, 1^1+3^2+5^3 = 135.
*/

split :: Int -> [Int]
split x
| x < 10 = [x]
= [x rem 10] ++ split(x/10)

//Start = sum [a^e \\ a<- reverse (split 786)& e<-[1..(length (split 786))]]


isDisariumNum :: Int -> Bool
isDisariumNum ls
| sum [a^e \\ a<- reverse (split ls)& e<-[1..(length (split ls))]] == ls = True
=False

//Start = isDisariumNum 135 // True
//Start = isDisariumNum 598 // True
//Start = isDisariumNum 518 // True
//Start = isDisariumNum 220 // False
//Start = isDisariumNum 110 // False


//// Harshad numbers
/*2.
Given a list of positive integer numbers, return a list that
contains the Harshad numbers of the list.
A Harshad number is an integer that is divisible by the sum of
its digits when written in that base.
Examples:
200 True, the sum of digits is 2+0+0=2 and 200 is divisible by 2.
171 True, the sum of digits is 1+7+1=9 and 171 is divisible by 9.
*/

/*
harshad :: Int -> Bool
harshad x
| x rem (sum (split x)) == 0 = True
= False
*/

//Start = harshad 200

harshadNums :: [Int] -> [Int]
harshadNums ls = filter (harshad) ls
	where harshad x
			| x rem (sum (split x)) == 0 = True
			= False
			
//Start = harshadNums ([8, 9, 10, 12, 18, 20, 21, 24, 27, 30] ++ [13..17]) // [8, 9, 10, 12, 18, 20, 21, 24, 27, 30]
//Start = harshadNums ([31..35] ++ [36, 17,40, 42, 45, 13, 48, 50, 54, 11, 60, 63]) // [36, 40, 42, 45, 48, 50, 54, 60, 63]
//Start = harshadNums [] // []


//// Leader numbers of a list
/*3.
Given a list of integer numbers, return all the leaders in the list.
A number is leader if it is strictly greater than all the elements
to its right side in a list.
Example: [10,9,14,23,15,0,9] -> [23,15,9]
23 is greater than all the numbers to its right 15,0,9.
15 is greater than all the numbers to its right 0,9.
9 there are no numbers in its right.
*/

//leaders :: [Int] -> [Int]


//Start = leaders [10,9,14,23,15,0,9] // [23,15,9]
//Start = leaders [1..10] // [10]
//Start = leaders [10,9..1] // [10,9,8,7,6,5,4,3,2,1]
//Start = leaders [7,8,10,9,5,3,6,4] // [10,9,6,4]
//Start = leaders [] // []


//// Replacing
/*4.
Given the list and a number K, remove all numbers that are divisible by K
and replace all other with reminder by K. Return resulting list.
Example: [1,3,8,6,2], K=3 -> [1,2,2]
3 and 6 are removed as they are divisible by 3.
1,8,2 are replaced with 1, 2, 2 reminders.
*/

divByK :: Int Int -> Bool
divByK x k
| x rem k == 0 = True
= False

//Start = divByK 10 5

filteredRem :: Int [Int] -> [Int]
filteredRem k ls = map (\x = x rem k) (filter (\x = not(divByK x k)) ls)

//Start = filteredRem 3 [1,3,8,6,2] // [1,2,2]
//Start = filteredRem 5 [5,10,30] // []
//Start = filteredRem 2 [2,8,3,4,1] // [1,1]
//Start = filteredRem 100 [20,17] // [20,17]


//// GoodNumbers
/*5.
Write a function that takes a list as an argument and
counts how many numbers are:
greater or equal to 10 AND less or equal to 99 AND divisible by 3.
*/

/*
goodNum :: Int -> Bool
goodNum x 
| (x >= 10) && (x<=99) && (x rem 3 == 0) = True
= False
*/

countGoodNums :: [Int] -> Int
countGoodNums ls = length (filter (goodNum) ls) 
	where goodNum x 
			| (x >= 10) && (x<=99) && (x rem 3 == 0) = True
			= False

//Start = filter (goodNum) [1,12,10,99]

//Start = countGoodNums [1,12,10,99] // 2
//Start = countGoodNums [12,15,30,33,39,96,99] // 7
//Start = countGoodNums [9, 10, 100, 102, 105] // 0
//Start = countGoodNums [] // 0


//// Valid Triangles
/*6.
Given a list of tuples, each with 3 numbers.
For each tuple check if these 3 numbers
can be used as sides of a triangle,
replace the tuple either with True or False.
3 numbers can be sides of triangles if each pair's
sum is greater than the remaining 3rd number.
A number cannot be a side if it is negative or 0.
*/


validTriangles :: [(Int,Int,Int)] -> [Bool]
validTriangles [] = []
validTriangles [(a,b,c) : xs]
| (a+b > c) && (b+c>a) && (c+a>b) = [True] ++ validTriangles xs
= [False] ++ validTriangles xs

//Start = validTriangles [] // []
//Start = validTriangles [(3,3,3), (2,4,5), (4,2,5), (3,3,10)] // [True, True, True, False]
//Start = validTriangles [(8,2,4), (3,10,3), (1,2,3)] // [False, False, False]
//Start = validTriangles [(10,8,3), (-10,4,2)] // [True, False]


//// Replicate
/*7.
Given a list of tuples, where each tuple contains a string and a number N.
For each tuple generate a list that contains N copies of the given string.
For negative number N generate empty list.
Example: the tuple ("ab", 3) should be replaced with ["ab","ab","ab"].

*/

rep :: String Int -> [String]
rep x 0 = []
rep x n 
| n< 0 = []
= [x : rep x (n-1)]

stringCopy :: [(String,Int)] -> [[String]]
stringCopy [] = []
stringCopy [(a,b): xs] = [rep a b] ++ stringCopy xs

//Start = stringCopy [("X",3),("AA",2)] // [["X","X","X"],["AA","AA"]]
//Start = stringCopy [("Clean", 1),("?!",0),("Empty",-1)] // [["Clean"],[],[]]
//Start = stringCopy [] // []


//// Integers' insertion
/*8.
Given two integers, insert the second integer into the first one.
After each digit considered in the first integer, insert a digit
from the second integer. Both given numbers are of equal length.
Example: 123 321 -> 132231
13 13 -> 1133
*/

//Start = flatten [1..5

listToInt :: [Int] -> Int
listToInt ls = sum [e*10^(a) \\ e<-ls & a<- reverse [0..((length ls)-1)] ]

//Start = listToInt [1..5]

intInsertion :: Int Int -> Int
intInsertion a b = listToInt (flatten [[x]++[y] \\ x<-(reverse (split a)) & y<-(reverse (split b))])

//Start = intInsertion 123 123 // 112233
//Start = intInsertion 123 321 // 132231
//Start = intInsertion 13 13 // 1133
//Start = intInsertion 1 2 // 12
//Start = intInsertion 2 1 // 21


//// Failed-passed students
/*9.
Given list of tuples and an integer value representing the 'pass_marks',
each tuple represents a student (name,marks), write a function which
groups the students into two categories based on their marks obtained in a test.
The function should return a tuple containing the list of the students
who failed, and the list of the students who passed.
Example:
List: [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)], pass_marks: 30
Output: ([("Ramesh",23)],[("Vivek",40), ("Harsh",88), ("Mohammad",60)])
--failed-- -------------passed------------------------
'Ramesh' failed as his marks 23 are less than the given number 30, all others passed.
*/

/*
group_by_marks :: [(String, Int)] Int -> ([(String,Int)], [(String,Int)])
group_by_marks [] x = ([])
group_by_marks [(a,b) : xs] k = ([(x,y) \\x<-unzip(a,b)],[()])
*/

//Start = unzip [("Ramesh",50),("Vivek",20),("Harsh",10),("Abdullah",90),("Mohammed",30),("Ahmed",0),("Othman",70)] 
// ([("Vivek",20),("Harsh",10),("Mohammed",30),("Ahmed",0)],[("Abdullah",90),("Othman",70)])

//Start = group_by_marks [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)] 30
// ([("Ramesh",23)],[("Vivek",40),("Harsh",88),("Mohammad",60)])
//Start = group_by_marks [("Ramesh",50),("Vivek",20),("Harsh",10),("Abdullah",90),("Mohammed",30),("Ahmed",0),("Othman",70)] 50
// ([("Vivek",20),("Harsh",10),("Mohammed",30),("Ahmed",0)],[("Abdullah",90),("Othman",70)])
//Start = group_by_marks [] 1 // ([],[])


//// Ciphering
/*10.
Given a list of characters, extract all the vowels and count them.
After that, cipher the list of characters by that count.
Ciphering here means just shift the character by that count.
English vowels are: a, e, i, o, and u.
Example: let's assume that the vowels' count is 2, then:
'a' + 2 = 'c' ... Here we ciphered 'a' into 'c'
'c' + 2 = 'e' ... We did the same as above
For the input ['m', 'o', 'h', 'i','d','o'] count of vowels is 3 o,i,o.
Cipher of the list: ['m', 'o', 'h', 'i','d','o']->['p','r','k','l','g','r'].
*/

countVowel :: [Char] -> Int
countVowel [] = 0
countVowel [ls : lss]
| isMember ls ['a','e','i','o','u'] = 1 + countVowel lss
= countVowel lss

//Start = countVowel ['m', 'o', 'h', 'i','d','o']

cipherList :: [Char] -> [Char]
cipherList [] = []
cipherList ls = [ toChar((toInt (a))+(countVowel ls)) \\ a<-ls ]

//Start = cipherList ['m', 'o', 'h', 'i','d','o'] // ['p','r','k','l','g','r']
//Start = cipherList ['t', 'a', 'r', 'i', 'q'] // ['v','c','t','k','s']
//Start = cipherList ['b', 'e', 'k', 'a'] // ['d','g','m','c']
//Start = cipherList ['a','b','d','u','l','l','a','h'] // ['d','e','g','x','o','o','d','k']


//// Reachable points
/*11.
Given coordinates of a source point (x1, y1) determine if it is possible to reach
the destination point (x2, y2). All coordinates are positive.
From any point (x, y) there are only two types of valid movements: (x, x + y)
and (x + y, y). Return a Boolean True if it is possible, else return False.
Example: source point: (2, 10)
destination point: (26,12)
output: True (2, 10)->(2, 12)->(14, 12)->(26, 12) is a valid path.
*/

//isReachable :: (Int,Int) (Int,Int) -> Bool


//Start = isReachable (2, 10) (26, 12) // True
//Start = isReachable (4, 20) (52, 24) // True
//Start = isReachable (8, 40) (104,48) // True
//Start = isReachable (6, 12) (20, 10) // False
//Start = isReachable (3, 15) (58, 69) // False


//// Evaluate
/*12.
Given a list of integer numbers representing coefficients of a polynomial,
the polynomial coefficients are given in increasing order of power.
Implement a function which evaluates the polynomial
according to a given value (substituting the given value into it).
You can assume that the given list is not empty.
Example:
List: [2,3,-5,1], the polynomial is 2x^0 + 3x^1 - 5x^2 + 1x^3.
Given value is 1: 2 + (3 * 1) + (-5 * 1^2) + (1 * 1^3) = 1.
*/

evaluate :: [Int] Int -> Int
evaluate [] x = 0
evaluate ls x = sum [a * (x^e) \\ a<-ls & e<-[0..(length ls)]]

//Start = evaluate [2,3,-5,1] 1 // 1
//Start = evaluate [1,-5,2,-8] -2 // 83
//Start = evaluate [1,1,1,1,1,1,1,1] 1 // 8

 

//// Moving digit
/*13.
Complete the function Mover that takes three integers: init, digit and target
and calculates the amount of places the digit, that is equal to the digit in
the number init, has to be shifted to the right in order to get the target number.
It is guaranteed that the digit exists in init and has to be shifted to the right.
Example: 134442 3 144423 -> 4
The digit 3 exists in the init number, and it has to be moved 4 places
in order to get the target number.
*/

/*
findIndex :: [Int] Int -> Int
findIndex [x:xs] a
| x <> a = 1 + findIndex xs a
= 0+ findIndex xs a
*/

//Start = findIndex (split 345) 5

//Start = length (takeWhile ((<>) 3) [1..5])

Mover :: Int Int Int -> Int
Mover a b c = abs((length (takeWhile ((<>) b) (split a))) - (length (takeWhile ((<>) b) (split c))))

//Start = Mover 123 2 132 // 1
//Start = Mover 134442 3 144423 // 4
//Start = Mover 100020001 2 100002001 // 1


/*
You are given a tuple of lower and upper bounds and a list of tuples, where the first element is course
code and the second one is course score. Filter the list and return the list of course codes, which have
scores greater or equal to the lower bound and less or equal to the upper bound.
*/

rangeFilter :: (Int, Int) [(String,Int)] -> [String]
rangeFilter a [] =[]
rangeFilter (a,b) ls = fst (unzip[(e,y)\\ e<-(fst(unzip ls)) & y<-(snd(unzip ls)) | y>=a && y<=b ])

//Start = fst(unzip[("A",12),("B",3),("C",5),("E",14),("F",16)])

//Start = rangeFilter (10, 15) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A","E"]
//Start = rangeFilter (3, 13) [("A",12),("B",3),("C",5),("E",14),("F",16)] // ["A", "B", "C"]
//Start = rangeFilter (5, 7) [] // []
//Start = rangeFilter (15, 3) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []
//Start= rangeFilter (0, 2) [("A",12),("B",3),("C",5),("E",14),("F",16)] // []


/*
2. Given a list of lists of Int.
For each element greater than 5 of each sublist create a triple like (number, sum, product)
where
number -> the number itself
sum -> the sum of all the integers in the [1,number) interval
product -> product of all the integers in the [1,number)

Example:
[[1,2,4],[6,8,2,1]] -> [[],[(6,15,120),(8,28,5040)]]
because 1,2,4,2,1 are not greater than 5,
15 = 1+2+3+4+5, 120 = 1*2*3*4*5
28 = 1+2+3+4+5+6+7. 5040 = 1*2*3*4*5*6*7
*/

numSumProdAux::Int->[(Int,Int,Int)]
numSumProdAux 0 = []
numSumProdAux x
| x < 5 = []
= [(x,foldr (+) 0 [1..x],foldr (*) 1 [1..x])]

numSumProd::[[Int]]->[[(Int,Int,Int)]]
numSumProd [] = []
numSumProd ls = flatten(map (map (numSumProdAux)) ls)

//Start = map (map (numSumProdAux)) [[1,2,4],[6,8,2,1]]
//[[],[(6,21,720),(8,36,40320)]]]
//Start = numSumProd [[1,2,4],[6,8,2,1]]//[[],[(6,21,720),(8,36,40320)]]
//Start = numSumProd [[1..6],[1..5]]//[[(6,15,120)],[]]
//Start = numSumProd [[1,2,2],[]]//[[],[]]
//Start = numSumProd []//[]

