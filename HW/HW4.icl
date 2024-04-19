module HW4
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
You are given a list of resultsTuples, where each tuple represents a 
team's performance in matches. 

Each tuple contains three integers:
1. The number of times the team won in a match (a).
2. The number of times the team played in drawn matches (tied) (b).
3. The number of times the team lost in a match (c).

Your task is to calculate the total points earned by each team based on the following rules:

- A team earns 3 points for each match won.
- A team earns 1 point for each match that ends in a draw.
- A team earns 0 points for each match lost.

Write a function teamWithHighestScore that takes a list of resultsTuples 
as input and returns the index of the team with the highest score. 
Teams are represented by their positions in the list (0-indexed).

Example:
Input: teamWithHighestPoints [(4, 7, 2), (3, 8, 1), (5, 5, 4)]
Expected Output: 2
Team 0 earns 4*3 + 7*1 + 2*0 = 19 points.
Team 1 earns 3*3 + 8*1 + 1*0 = 16 points.
Team 2 earns 5*3 + 5*1 + 4*0 = 20 points.
Team 2 has the highest points, so the function returns 2.
*/

scoreCalculator :: (Int,Int,Int) -> Int
scoreCalculator (a,b,c) = (3*a) + (1*b) + (0*c)

indexGetter :: (Int,[Int]) -> Int
indexGetter (a,ls) = a

teamWithHighestScore :: [(Int, Int, Int)] -> Int
teamWithHighestScore x = indexGetter (removeIndex (last b) (map scoreCalculator x))
where b = sort (map scoreCalculator x) 

//Start = teamWithHighestScore [(4, 7, 2), (3, 8, 1), (5, 5, 4)] // 2
//Start = teamWithHighestScore [(10, 0, 0)] // 0
//Start = teamWithHighestScore [(2, 1, 0), (1, 2, 0), (0, 3, 0), (2, 1, 0)] // 0
//Start = teamWithHighestScore [(3, 5, 1), (2, 6, 2), (4, 4, 3)] // 2
//Start = teamWithHighestScore [(0, 0, 5), (1, 1, 3), (2, 2, 2)] // 2


/*
Problem2:
Write a function productOfSquaresAndCubes that takes a list of integers 
and calculates the product of the squares of even integers and the cubes of odd integers 
in the list using the foldr function. If the result of the calculation exceeds 
a threshold of �100,0000, the function should terminate and return an error message.
*/

sqCubeCalc :: [Int] -> [Int]
sqCubeCalc [] = []
sqCubeCalc [x:xs]
| isEven x = [(x*x): sqCubeCalc xs]
| otherwise = [(x*x*x) : sqCubeCalc xs]

productOfSquaresAndCubes :: [Int] -> Int
productOfSquaresAndCubes ls 
| a <= -100000 = abort "Result exceeds the threshold of -100,0000!"
| a >= 100000 = abort "Result exceeds the threshold of +100,0000!"
= a
where a = (foldr (*) 1 (sqCubeCalc ls))

//Start = productOfSquaresAndCubes [1, 2, 3, 4, 5] //216000 //4 * 16 * 1 * 27 * 125 = 216000 // Result exceeds the threshold of +100,0000!
//Start = productOfSquaresAndCubes [] //1
//Start = productOfSquaresAndCubes [-1, 0, 1, 2, 3] //0 //0 * 4 * (-1) * 1 * 27 = 0
//Start = productOfSquaresAndCubes [100, 200, 300, 400, 500] //Result exceeds the threshold of -100,0000
//Start = productOfSquaresAndCubes [101, 201, 301, 401, 501] //Result exceeds the threshold of -100,0000
