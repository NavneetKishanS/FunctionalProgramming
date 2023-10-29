module ex4
import StdEnv

// Exercises

// 1. Write a recursive function that computes the n-th multiple of an x plus 10 (n*x+10).
f1 :: Int Int -> Int
f1 0 x = 10
f1 n x = x + f1 (n-1) x

//Start = f1 5 2 // 20



// 2. Add 2 to every odd number of a list, and subtract 2 from every even number.
f2 :: [Int] -> [Int]
f2 [] = []
f2 [x:xs]
| isOdd x = [x+2 : f2 xs]
= [x-2 : f2 xs]

//Start = f2 [1..5] // [3,0,5,2,7]



// 3. Compute the triple of the negative elements of a list up to the first positive number.
f3 :: [Int] -> [Int]
f3 [] = []
f3 [x:xs]
| x < 0 = [3*x : f3 xs]
= []

//Start = f3 [-1,-3,-5,-5,2,-4,-5] // [-3, -9, -15, -15]



// 4. Write a function that keeps the non-zero elements of a list and then multiply by 2 every element.
f4 :: [Int] -> [Int]
f4 [] = []
f4 ls = map (\x = x*2) (filter ((<>) 0 )ls)

//Start = f4 [1,2,3,0,5,0,6,0,0,0,0] // [2,4,6,10,12]



// 5. Write a function for the square, the cube, and so on up to the n-th power of a number,
// so that increasing powers of a number are obtained in a list.
f5 :: Int Int -> [Int]
f5 n x
| n == 1 = [x]
 = sort(reverse([ x^n : f5 (n-1) x ]) ) 

Start = f5 5 2  // [4,8,16,32] ////DOUBT



// 6. Replicate n>0 times a list.
f6 :: Int [Int] -> [[Int]]
//f6 0 [] = [[]]
f6 n ls = map(\x = rep n x ) ls

rep :: Int Int -> [Int]
rep 0 x = []
rep n x = [ x : rep (n-1) x ]

//Start = f6 3 [1..5] // [[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]



// 7. Insert 0 at the middle of each sublist. ////DOUBT
f7 :: [[Int]] -> [[Int]]
f7 x = map ( insertAt mid 0) x 
where mid = ((length x)/2)

//Start = f7 [[1..10], [1..11], [], [1], [1,2]] // [[1,2,3,4,5,0,6,7,8,9,10],[1,2,3,4,5,0,6,7,8,9,10,11],[0],[0,1],[1,0,2]]



// 8. Extract the elements smaller then the head element of a list.
f8 :: [Int] -> [Int]
f8 ls = map (\x = x) (filter ((>) head)ls)
where head = hd ls

//f8a :: Int [Int] -> [Int]

//



//Start = f8 [5,1,2,3,4,5,3,6,7,1,8] // [1,2,3,4,3,1]


// 9. Eliminate in a list the sublists that are longer or equal 10.
cond9 :: [Int] -> Bool
cond9 x = length x < 10

//f9 :: [[Int]] -> [[Int]]
//f9 ls = map( cond9 (ls)) ls
//f9 [] = []
//f9 [x : xs]
//| cond9 x == False = [ x : f9 xs]
//=[ x : f9 xs]

//Start = f9 [[1..10], [1..11], [1..5], []] // [[1,2,3,4,5],[]]



// 10. Compute the greatest common divisor in a recursive function.
f10 :: Int Int -> Int
f10 a b
#c = (a rem b)
| (b<0) || (a<0) = abort "nums must be +ve."
| (b==0) = a
= gcd b c

//Start = f10 24 12


// 11. Given a list of Ints, remove the element at the given position.
remElemAt :: Int [Int] -> [Int]
remElemAt n x = map (removeAt n x) x

//remElemAt2 :: Int [Int] -> [Int]
//remElemAt2 i list = removeAt i list

//Start = remElemAt 6 [1..7] // [1,2,3,4,5,6]
//Start = remElemAt 2 [1..7] // [1,2,4,5,6,7]
//Start = remElemAt 9 [1..7] // [1,2,3,4,5,6,7]




// 12. Given a list of integers, find the minimum of a list (assume the list is not empty). 
//minimum :: [Int] -> Int 

/*
[1,0,3,4,5]
minimum [0,3,4,5]
minimum [0,4,5]
minimum [0,5]
minimum [0]
*/

//Start = minimum [1..5] // 1
//Start = minimum [10,9,8,7,6] // 6
//Start = minimum [8,6,4,10,12] // 4

minimum2 :: [Int] -> Int 
minimum2 [x] = x
minimum2 [x:xs] = min x (minimum2 xs)

/*
minimum2 [1,2,0,-1] -> min 1 (min 2 (min 0 (-1)))
minimum2 [1,2,0,-1] -> min 1 (min 2 -1)
minimum2 [1,2,0,-1] -> min 1 -1
minimum2 [1,2,0,-1] -> -1
*/

//Start = minimum2 [1..5] // 1
//Start = minimum2 [10,9,8,7,6] // 6
//Start = minimum2 [8,6,4,10,12] // 4

minimum3 :: [Int] -> Int 
minimum3 x = minList x

//Start = minimum3 [1..5] // 1

// 13. Check if an element of a logical list is true.
//ifOneTrue :: [Bool] -> Bool

//Start = ifOneTrue [False, False, False] // False
//False || False || False || False



// 14. Check if all elements of a logical list are true.
//ifAllTrue :: [Bool] -> Bool

//Start = ifAllTrue [True, False, True] // False
//True && False && True  && True



// 15. Write a function that checks if at least one of the elements in a list is even.
//is_one_Even :: [Int] -> Bool 

//Start = is_one_Even [1,1,3] // False
//Start = is_one_Even [1..9] // True
//Start = is_one_Even [2,4..14] // True
//Start = is_one_Even [] // False



// 16. Write a function that checks if all of the elements in a list are even.
//allEven :: [Int] -> Bool
  
//Start = allEven [2,4,6]
// [2,4,6] -> isEven 2 && isEven 4 && isEven 6 && True 
//Start = allEven [1..9] // False
//Start = allEven [2,4..14] // True
//Start = allEven [] // True



// 17. Collect the divisors of a number in a list.
divisorsAux :: Int Int [Int] -> [Int]
divisorsAux inc stop accum
| inc > stop = accum
| stop rem inc  == 0 = divisorsAux (inc+1) stop (accum ++ [inc]) 
= divisorsAux (inc+1) stop accum

divisors :: Int -> [Int] 
divisors number = divisorsAux 1 number [] 

//Start = divisors 18 // [1,2,3,6,9,18]


Divisors :: Int Int -> [Int] 
Divisors inc stop
| inc > stop = []
| stop rem inc == 0 = [inc] ++ Divisors (inc+1) stop
= Divisors (inc+1) stop 

/*
Divisors 9 1 
/*| 1 == 9  = False */
| 9 / 1 == 0  True ->  [1] ++ [3] ++ [] = [1,3] 
Divisors 9 2
/*| 9 / 2 == 1 (False ) ->*/
Divisors 9 3 
Divisors 9 4 
Divisors 9 5 
.. etc 
Divisors 9 9 = []
*/

divisors2 :: Int -> [Int] 
divisors2 number = Divisors 1 number

//Start = divisors2 18 // [1,2,3,6,9,18]


// 18. Delete every second element from a list.
//del2 :: [Int] -> [ Int]

//Start = del2 [1..10] // [1,3,5,7,9]
//Start = del2 [1..11] // [1,3,5,7,9,11]




// 19. Delete every third element of the sublists of a list.
//f19 :: [Int] -> [Int]

//Start = f19 [1..30]

delete_3 :: [Int] -> [Int]
delete_3 [] = [] 
delete_3 x = removeAt 2 x

//del3  :: [[Int]] -> [[Int]]

//Start = del3 [[1..5],[],[1..4],[1,5],[1],[1..3],[1..10]]

// [[1,2,4,5],[],[1,2,4],[1,5],[1],[1,2],[1,2,4,5,7,8,10]]



// 20. Check if a list contains 2 equal elements one after the other
// (they can be anywhere in the list) and count such equalitites
// for [1,2,2,3,4,3,3,2,4,5,5,5] is 4 for [1 .. 5] is 0.
fe :: [Int] -> Int
fe [] = 0
fe [h] = 0
fe [h,x:t]
| h == x = 1 + fe [x:t]
= fe [x:t]

//Start = fe [1,2,2,2,3,4,3,3,2,4,5,5,5,5,5,5,5,5,5]



// 21.* (bonus) Compute the Euler number aproximation in n steps: e = 1/0! + 1/1! + 1/2! + 1/3! + ... 
// do not compute factorial 
f21 :: Int -> Real
f21 n
| n ==0 = 1.0
 = toReal(1/factor n) + toReal(f21(n-1)) 

factor :: Int -> Int
factor n
| n == 0 || n== 1  = 1
= n * factor(n-1)

//Start = f21 1000

/*
fhalf :: Int -> Bool
fhalf x
| isOdd(length x) = False
= isPali (split x)

isPali :: [Int] -> Bool
isPali x = [split x] ++ [split(x/10)]

split :: Int -> [Int]
split x
| x<10 = [x]
= [x rem 10] ++ split (x/10)

Start = fhalf 1212 */

/*
g :: Int -> Bool
g x = split(x)

split :: Int -> [Int]
split x
| x<10 = [x]
= [x rem 10] ++ split (x/10)

h :: [Int] -> [Int] [Int]
h ls = splitAt ( ((length (ls))/2) ls)
*/

//EXTRA - Othman Abdullah
/*
1.
    * Given a positive integer number, check if the given number is a Disarium number or not.
    * A Disarium number is a number defined by the following process :
    * Sum of its digits powered with their respective position is equal to the original number.
    * For example 135 is a Disarium number :
    * As 1^1+3^2+5^3 = 135
*/
/*
split :: Int -> [Int]
split x
| x<10 = [x]
= [x rem 10] ++ split (x/10)

disarium :: [Int] -> Bool
disarium x = sum[x^i : 
where i = [1..]
//disarium x = sum [x^i \\ x <- (split x) & i <- [1..]] == x


*/

/*
split :: Int -> [Int]
split x
| x<10 = [x]
= [x rem 10] ++ split (x/10)

sumOddDigits :: Int -> Int 
sumOddDigits x = map (

*/