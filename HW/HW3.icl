module HW3
import StdEnv

/*
Please write your neptun code here: JZD5GY
submitted by : Navneet Kishan Srinivasan - JZD5GY

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
Create a function customTransformAndFilter that takes a list of integers,
a transformation value, and returns a new list containing the filtered 
and transformed elements.

Example:
customTransformAndFilter [1, 2, 3, 4, 5] 2

The customTransformAndFilter function first filters the even elements 
from the list and then applies the transformation value of 2 
to each element, resulting in the output [4, 8].
*/


transformer :: [Int] Int -> [Int]
transformer [] n= []
transformer [x:xs] n
| isOdd(x) = [1 : transformer xs n]
=[ x*n : transformer xs n]

customTransformAndFilter :: [Int] Int -> [Int]
customTransformAndFilter ls n = filter ((<>) 1) (transformer ls n)

//Start = transformer [1, 2, 3, 4, 5] 2

//Start = customTransformAndFilter [1, 2, 3, 4, 5] 2 //[4, 8]
//Start = customTransformAndFilter [] 3 //[]
//Start = customTransformAndFilter [2, 4, 6, 8, 10] 3 //[6, 12, 18, 24, 30]
//Start = customTransformAndFilter [1, 3, 5, 7, 9] 5 //[]
//Start = customTransformAndFilter [1..100] 2 //[4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80,84,88,92,96,100,104,108,112,116,120,124,128,132,136,140,144,148,152,156,160,164,168,172,176,180,184,188,192,196,200]

/*
Problem2:
Write a function recursiveListSubtraction that takes two lists of integers as input: a source list and a list of elements to subtract. 

Example:
recursiveListSubtraction [1, 2, 3, 4, 5] [2, 4]

The function iterates through the source list [1, 2, 3, 4, 5] and 
subtracts elements [2, 4]. It includes elements that are not 
in the subtract list, resulting in [1, 3, 5].
*/


recursiveListSubtraction :: [Int] [Int] -> [Int]
recursiveListSubtraction [] [] = []
recursiveListSubtraction [] ls2 = []
recursiveListSubtraction ls1 [] = ls1
recursiveListSubtraction x [y:ys] = recursiveListSubtraction (filter ((<>) y) x) ys


//Start = recursiveListSubtraction [1, 2, 3, 4, 5] [2, 4] //[1, 3, 5]
//Start = recursiveListSubtraction [10, 20, 30, 40, 50] [] //[10, 20, 30, 40, 50]
//Start = recursiveListSubtraction [1, 2, 3, 4, 5] [1, 2, 3, 4, 5] //[]
//Start = recursiveListSubtraction [] [2, 4] //[]
//Start = recursiveListSubtraction [1, 2, 2, 3, 4, 4, 5] [2, 4] //[1, 3, 5]