module ex7

import StdEnv


// 1. generate a list with 10 times of 13 : [13,13,13,13,13,13,13,13,13,13]
l1 :: [Int]
l1 = [13 \\ e <- [1..10]]

//__________NOTES______________________________________________________________________________
//e<-[1...10] === equivalent to forEach loop
// , comma === cartesian prod
// & and === iterates simultaneously until the length of the shorter list
//_____________________________________________________________________________________________


//Start = l1


// 2. generate the following list [(1,1),(1,2),(2,1),(2,2)]
l2 :: [(Int, Int)]
l2 = [(a,b) \\ a <- [1..2], b <- [1..2]]
//Start = l2


// 3. generate the following list [(1,3),(1,2),(1,1),(2,3),(2,2),(2,1),(3,3),(3,2),(3,1)]
l3 :: [(Int, Int)]
l3 = [(a,b) \\ a <- [1..3] & b <- [3,2,1]]

//Start = l3


// 4. generate the list [(1,5),(2,6),(3,7),(4,8),(5,9),(6,10)]
l4 :: [(Int, Int)]
l4 = [(a,b) \\ a <- [1..6] & b <- [5..10]]

//Start = l4

//__________NOTES_______________________________________________________________________________
// fst === first element of tuple
// snd === second element of tuple
//repeatn === built-in function to repeat an element n times
//_____________________________________________________________________________________________

// 5. generate the list [1,2,2,3,3,3,4,4,4,4,...,10,..,10]
l5 :: [Int]
l5 = [ y \\  y <-[1..10], x <- [1..y]]
l51 = [ snd (x,y) \\  y <-[1..10], x <- [1..y]]
l52 = flatten [ repeatn y y \\  y <-[1..10]]

//Start = l5


// 6. generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
l6 :: [[Int]]
l6 = [ [y \\ x <- [1..y] ] \\ y <-[1..10]]
l62 = [ repeatn y y \\  y <-[1..10]]

//Start = l62
  
  
// 7. generate 100 pythagoras numbers : [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
l7 :: [(Int, Int, Int)]
l7 =  take 100 [(a,b,c) \\ c <- [1..], b<-[1..c], a<-[1..b] | a*a + b*b == c*c]
// just c is restricted, not the number of pairs
l71 = [(a,b,c) \\ c <- [1..100], b<-[1..c], a<-[1..b] | a*a + b*b == c*c]
// a,b,c restricted but not the nr of pairs
l72 =  take 100 [(a,b,c) \\ a<-[1..100], b<-[1..100],c <- [1..100]  | a*a + b*b == c*c && a<b && b<c]

//Start = l7


// 8. Generate 100 even numbers using list comprehensions
l8 :: [Int]
l8 = take 100 [ a \\ a <- [1..200] | a rem 2 == 0]

//Start = l8


// 9. Generate the following list [4, 16, 36, 64, 100, 144, 196, 256, 324, 400]
l9 :: [Int]
l9 = [ a*a \\ a<-[1..20] | a rem 2 == 0]

//Start = l9
//Start = l92


// 10. List powers of 2 from 1 to 10.
// hint: use x^y (x at power y)
l10 :: [Int]
l10 = [ 2^x \\ x<-[1..10]]
l10_v2 = take 10[ 2^x \\ x<-[1,2..]]

//Start = l10_v2


// 11. List the divisors of 90.
l11 :: [Int]
l11 = [ a \\ a<-[1..90] | 90 rem a ==0]

//Start = l11


// 12. List “dominoes”: [(0,0),(0,1),(1,1),(0,2),(1,2),(2,2),...,(9,9)]
// Domino (1,0) is not in the list because it is already in it as (0,1).

//l12 :: [(Int, Int)]
l12= [(b,a) \\ a<-[0..9], b<-[0..a]]

//Start = l12


// 13. Construct the list [(1,'a'),(2,'b'),…(…,'z')], i.e. pair up numbers with abc letters.
l13 :: [(Int, Char)]
l13 = [(a,b) \\ a<-[1,2..] & b<-['a'..'z']]

//Start = l13


// 14. Generate a list of length 10 whose elements are False, True, False, True, … (alternating)
l14 :: [Bool]
l14 = take 10[isEven a \\ a<-[1,2..]]

//Start = l14


// 15. Is 123457 a prime number? A nr is prime if only 1 and the numebr divides it.
//l15 :: Bool
//l15 = [ length  \\ a<-[1..90] | 90 rem a ==0 ]
//Start = l15


// 16. Generate the list [(0,10),(1,9),…,(10,0)].
l16 :: [(Int, Int)]
l16 = [(a,b) \\ a<-[0..10] & b<-[10,9..0]]

//Start = l16


// 17. Generate a list that contains all (hour, minute) pairs in a day.
l17 :: [(Int, Int)]
l17 = [(a,b) \\ a<-[1..24] , b<-[0..59]]

//Start = l17

// 18.  generate the following list [(1,1),(2,2),(3,3),(4,4),(5,5)]
l18 :: [(Int, Int)]
l18 = [(a,a) \\ a<-[1..5]]

//Start = l18


// 19. generate [(1,2,3),(2,4,6),(3,6,9),(4,8,12),(5,10,15)] 
l19 :: [(Int, Int, Int)]
l19 = [(a,a*2,a*3) \\ a<-[1..5]]

//Start = l19


// 20. Compute the sum of the list of tuples [(1,1), (2,2), (3,3)] -> (6,6)
//sumtup :: [(Int, Int)] -> (Int, Int)
//sumtup a = [flatten(sum (unzip a))] 

//Start = sumtup [(1,1), (2,2), (3,3)]

// 21. Generate 5 tuples like [(1,2),(2,3),(3,4),(4,5),(5,6)]
increase :: [(Int, Int)]
increase = [(a,a+1) \\ a<-[1..5]]

//Start = increase


// 22. Make triple tuples like [(1,2,3),(4,5,6),(7,8,9),(10,11,12),(13,14,15)]
tripl :: [(Int, Int, Int)]
tripl = [(a,a+1,a+2) \\ a<-[1,4,7,10,13]]

//Start = tripl


// 23. Given a list of lists, transform it tuples of sublist such that two 
// continous sublists form pairs 
// (if there are odd number of sublist the last has as pair the empty list)
//pairs :: [[Int]] -> [([Int],[Int])]

//Start = pairs [[1,2,3], [5,6], [7,8,9,10], [11,3], [1..5]]
//Start = pairs [[1,2,3], [5,6], [7,8,9,10], [11,3]]


// 24. Given a list of tuples form a list of triple tuples with the original 
// numbers and their sum
triplesum :: [(Int, Int)] -> [(Int, Int, Int)]
triplesum [] = []
triplesum [(a,b):ls] = [(a,b,(a+b))] ++ triplesum ls

//Start = triplesum [(1,2)]
//Start = triplesum [(1,2),(2,3),(3,4),(4,5),(5,6)]


// 25. Generate quadruples of a number, its square, its cube, and its biquadratic
// where the number are in the 1..20 interval
quadruple :: [(Int, Int, Int, Int)]
quadruple = [(a, a^2, a^3, a^4) \\ a<- [1..20]]

//Start = quadruple


// 26. Form triple tuples of 3 lists selecting one element from each list.
// E.g. for ([1..10],[20..25],[35..47]) the result is 
// [(1,20,35),(2,21,36),(3,22,37),(4,23,38),(5,24,39),(6,25,40)]
tri :: ([Int],[Int],[Int]) -> [(Int, Int, Int)]
tri (x,y,z) = [(a,b,c) \\ a<-x & b<-y & c<-z ]


//Start = tri ([1..10],[20..25],[35..47])


// 27. write a function duplicates which checks if there are neighbour duplicates in a list
duplic :: [Int] -> Bool
duplic [] = False
duplic [x] = False
duplic [x, y : t]
| x == y = True
= duplic [y:t] 

//Start = duplic [1, 1] // True
//Start = duplic [2] // False
//Start = duplic [1, 2, 3, 4, 5, 6, 7, 8, 9] // False
//Start = duplic [1, 0, 5, 0, 0, 6, 7, 5, 0, 0, 0, 8, 0, 5, 0, 0, 0] // True
//Start = duplic [1,2,3,4,4] // True


// 28. write a function that removes neighbour duplicates in a list
duplicrem :: [Int] -> [Int] 
duplicrem [] = []
duplicrem [x] = [x]
duplicrem [x, y : t]
| x == y = duplicrem [y:t]
= [x: duplicrem [y:t]]

//Start = duplicrem [1,1, 0, 5, 0, 0, 6, 0,0,0, 7, 5, 0, 0, 0, 0, 8, 0, 5, 0, 0, 0] 


// 29. transform the sub-sub lists into one list of sublists
f :: [[[Int]]] -> [[Int]]
f x = flatten x

//Start = f [[[1,2,3], [3,4,5]], [[1,2,3], [3,4,5], [7,8,9]]] 
// result : [[1,2,3],[3,4,5],[1,2,3],[3,4,5],[7,8,9]]



// 30. (bonus point) Generate a list that contains all (month, day) pairs in a 365-day 
/*
faux :: Int -> [Int]
faux a 
| isMember a [4,6,9,11] = [1..30]
| a==2 = [1..28]
= [1..31]

l30 :: [(Int),(Int)]
l30 = [(m,d) \\ m<-[1..12], d <- faux m ]
Start = l31
*/

//hint use x,y,z genrators without aux functions
l30 :: [(Int),(Int)]
l30 = [(m,d) \\ m<-[1..12], d <- [1,2,3,x,5,x,7,8,x,10,x,12] ]
Start = l31