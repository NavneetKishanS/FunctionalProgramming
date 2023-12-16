module midterm3

import StdEnv 


// Please fill the data required below.
//<Name>
//<Neptun_code>
//Functional Programming & mid-term
//2021.September.14 
//This solution was submitted and prepared by <Name, Neptun_code> 
//for the mid-term assignment of the Functional Programming course.
//I declare that this solution is my own work.
//I have not copied or used third party solutions.
//I have not passed my solution to my classmates, neither made it public.
//Students’ regulation of Eotvos Lorand University 
//(ELTE Regulations Vol. II. 74/C.) 
//states that as long as a student presents another student’s work - 
//or at least the significant part of it - as his/her own performance, 
//it will count as a disciplinary fault. 
//The most serious consequence of a disciplinary fault can be 
//dismissal of the student from the University.


/* 1. Parasitic Number

 A Parasitic number (in base 10) is a positive number which can be multiplied 
 by a certain n by moving the rightmost digit of its decimal representation 
 to the front.
 e.g. 102564 × 4 = 410256
 Given a positive integer number and n, write a function to determine whether 
 it is a Parasitic number or not.
*/
parasitic :: Int Int -> Bool
parasitic a b 
| (a * b) == ((a rem 10)*(10^5) + (a/10)) = True
= False

//Start = (102564 rem 10)*(10^5) + (102564/10)
//Start = parasitic 102564 4 // True
//Start = parasitic 142857 5 // True
//Start = parasitic 714285 8 // False
//Start = parasitic 105263157894736842 2 // True



/* 2. Double Ones

 Given a list of integers, write a function which will keep only the numbers
 that contain at least two '1' digits. For example:
 [1,2,21,121,11,234131,111111,123,0,334] -> [121,11,234131,111111]
*/
//doubleOne :: [Int] -> [Int]

// Start = doubleOne [1,2,21,121,11,234131,111111,123,0,334] // [121,11,234131,111111]
// Start = doubleOne [12,1,11,33] // [11]
// Start = doubleOne [11,111,21] // [11,111]
// Start = doubleOne [] // []
// Start = doubleOne [21,3,1] // []


/* 3. Multiples

 Given an n>0 integer value, write a function that creates the double, the triple
 and so on n-th multiple of the number.
*/

multiple :: Int -> [Int]
multiple 1 = []
multiple 2 = [2]
//multiple x = [x*2] ++ multiple (x-1) 

multiple x= [x*n \\ n <- [2..x]]

//Start = multiple 5 // [10,15,20,25]
//Start = multiple 2 // [2]
//Start = multiple 1 // []


/* 4. List difference
 
 Given two lists (A and B) containing sublists of integer numbers, 
 both A and B are of the same length,
 for every sublist in A and B, return the difference of the two sublists.  

 The difference is defined as follows:  
 The List L1-L2 consists of elements that are in L1 but not in L2. 
 For example if L1=[1,2,3] and L2=[3,5], then L1-L2=[1,2].
*/

//difference :: [[Int]] [[Int]] -> [[Int]] 

//Start = difference [[1..5]] [[4..7]] // [[1,2,3]]
//Start = difference [[1..10] , [10..15] , [1..4]] [[1..10] , [11..20] , [5]] // [[],[10],[1,2,3,4]]
//Start = difference [] [] // [] 

 
/* 5. Replace middle

 Given a list of lists of integers and an integer, write a function that replaces 
 the middle element with the given integer in every sublist. 
*/

mid :: [Int] Int -> [Int]
mid [] n = [n]
mid ls n //=take ((length ls)/2) ls ++ [n] ++ reverse (take ((length ls)/2) (reverse ls))
//= ls !! ((length ls)/2)
| isEven(length ls) == True = take ((length ls)/2) ls ++ [(ls !!((length ls)/2))-(ls !!((length ls)/2)) + n] ++ reverse (take (((length ls)/2)-1) (reverse ls))
=take ((length ls)/2) ls ++ [(ls !!((length ls)/2))-(ls !!((length ls)/2)) + n] ++ reverse (take ((length ls)/2) (reverse ls))
//=ls !!((length ls)/2) - (ls !!((length ls)/2)) + n

//Start = mid [1..4] 2

repMid :: [[Int]] Int -> [[Int]]
repMid [[]] n = [[]]
repMid ls n = map (\ls = mid ls n) ls


//Start = repMid [[1,2,3],[1..4]] 10 // [[1,10,3],[1,2,10,4]]
//Start = repMid [[1..6], [9,8..1], [(-1),(-2)..(-10)]] 5 
          // [[1,2,3,5,5,6],[9,8,7,6,5,4,3,2,1],[-1,-2,-3,-4,-5,5,-7,-8,-9,-10]]
//Start = repMid [[1,3],[]] 5 // [[1,5],[5]]


/* 6. Primes7

 Given a list of numbers, keep only the prime numbers that end with the digit 7
*/

/*
isPrime :: Int -> Bool
isPrime n 
| n < 2 = False
= not (any (\x = n rem x == 0) [2..(n-1)])
*/

is7 :: Int -> Bool
is7 x 
| x rem 10 == 7 = True
= False

//Start = is7 15

primes7 :: [Int] -> [Int]
primes7 ls = filter ( is7 )(filter (isPrime) ls)

//Start = primes7 [1..10] // [7]
//Start = primes7 [1..100] // [7,17,37,47,67,97]
//Start = primes7 [1..6] // []


/* 7. Property check

 Given a list of tuples, write a function to determine
 whether all of the tuples inside of the list hold the (Even, Odd) property.
 [(2,1),(2,3),(4,1)] = True
*/

/*
holdsTrue :: [(Int, Int)] -> Bool
holdsTrue [] = False
holdsTrue [tup]//[(a,b) : (as:bs)] 
//| (isEven a)== True && (isOdd b) == True = True
| (isEven (fst tup)) && (isOdd (snd tup)) = True //: holdsTrue tups
= False
*/

/*
holdsTrue :: [(Int,Int)] -> Bool
holdsTrue [] = []
holdsTrue xs = all(\(a,b) = (isEven a) && (isOdd b)) xs
*/

//Start = holdsTrue [(2,1),(2,3),(4,1)] // True
//Start = holdsTrue [(1,3),(2,3),(3,4)] // False
//Start = holdsTrue [] // False


/* 8. Super Digit

 We define super digit of an integer x using the following rules.
 If x has only 1 digit, then its super digit is x.
 Otherwise, the super digit of x is equal to the super digit of the digit-sum of x.
 Here, the digit-sum of a number is defined as the sum of its digits.

 E.g  : super_digit(9875) = super_digit(9+8+7+5) 
                          = super_digit(29) 
                          = super_digit(2+9)
                          = super_digit(11)
                          = super_digit(1+1)
                          = super_digit(2)
                          = 2

 Given a list of integers, return a list containing the super digit
 of every number in the list.  
*/

split :: Int -> [Int]
split x
| x<10 = [x]
= [x rem 10] ++ split (x/10)

superCalc :: [Int] -> Int
superCalc [] = 0
superCalc ls 
| sum ls > 9 = superCalc (split (sum ls))
= sum ls

//Start = superCalc (split 9875)


super_digit :: [Int] -> [Int]
super_digit [] = []
super_digit ls = map (\x = superCalc (split x)) ls


//Start = super_digit [148148148 , 9875 ] // [3,2]
//Start = super_digit [884555 , 456 , 2351 , 21587 , 88 ] // [8,6,2,5,7]
//Start = super_digit [] // [] 


/* 9. Powers 
 Given a list of integers and an integer, write a function which returns a list 
 which only contains the powers of the integer.
*/

//DOUBT
isPowerOf :: Int Int -> Bool
isPowerOf x n = iter 1 n where
    iter acc n 
        | acc == x = True
        | acc > x = False
        | otherwise = iter (acc*n) n

powersList :: [Int] Int -> [Int]
powersList [] n = []
powersList xs n = filter (\y -> isPowerOf y n) xs

//powersList :: [Int] Int -> [Int]

//Start = powersList [2,4,8,16,32,33,55] 2 // [2,4,8,16,32]
//Start = powersList [] 3 // []
//Start = powersList [1..10] 3 // [1,3,9]
//Start = powersList [-1,-2,4,8] 4 // [4]


/* 10. Twin primes
 
 Twin primes is a pair of primes, such that it contains a prime number that is either 
 2 less or 2 more than the pair prime number.
 For example, (41, 43) is a twin prime pair.
 Given a range of numbers left..right write a function that returns the count of 
 twin primes within the range.

 E.g: between 1 and 50 there are 6 pairs of twin prime numbers:
 [(3,5),(5,7),(11,13),(17,19),(29,31),(41,43)].
*/

isPrime :: Int -> Bool
isPrime n 
| n < 2 = False
= not (any (\x = n rem x == 0) [2..(n-1)])

keepPrimes :: Int Int -> [Int]
keepPrimes m n = filter ((isPrime))[m..n]

//Start =keepPrimes 1 7 

twinPrimes :: Int Int -> Int
twinPrimes x y = length [(a,b) \\ a<-init (keepPrimes x y), b <-(sort (init(reverse (keepPrimes x y)))) | b-a == 2]

//twinPrimes :: Int Int -> [(Int,Int)]
//twinPrimes x y = [(a,b) \\ a<-init (keepPrimes x y), b <-(sort (init(reverse (keepPrimes x y)))) | b-a == 2]

//Start = length (primePair (keepPrimes 1 7 ))
//Start = [1..5] !! 1

//twinPrimes :: Int Int -> Int
//twinPrimes m n = keepPrimes m n

//Start = twinPrimes 1 50 // 6
//Start = twinPrimes 1 1000 // 35
//Start = twinPrimes 0 2 // 0 doesn't work
//Start = twinPrimes 0 -5 // 0

