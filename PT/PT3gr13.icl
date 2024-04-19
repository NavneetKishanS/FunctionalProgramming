module PT3gr13
import StdEnv

/*
Please write your name and neptun code here: 

In our evaluation process, we maintain a strict policy against plagiarism
and the use of high-performance answers in ChatGPT responses.
Any instances of these will be assigned a score of zero, without exception. 

Your submission should not have any errors when running the code.
You'll receive a total of 100 points when you successfully solve a problems,

It is possible to get partial points for not working code, but please make sure you comment out the errors.

You should not delete anything from the given code, given test cases should stay the same, but you can add 
your tests as well. Don't change the given function signatures, however, you can add as many functions as 
you wish, just make sure to name them appropriately (if function squares the number, call it 'square',
'second_power', etc. and not 'f' or 'g'). The same goes for variable names. 

Make sure that you comment all 'Start'-s before submitting the code.
*/

/*
Given a list of integers and an integer k, 
write a function findFirstOccurrence that returns the index (0-based)
of the first occurrence of k in the list. 
If k is not present in the list, return -1.
*/

f :: (Int,[Int]) -> Int
f (x,y)= x

findFirstOccurrence :: [Int] Int -> Int
findFirstOccurrence [] _ = -1
findFirstOccurrence x k
| isMember k x = f (removeIndex k x)
= -1



//Start = findFirstOccurrence [4, 2, 3, 1, 2, 3, 4, 2] 2 //1
//Start = findFirstOccurrence [1, 1, 1, 1, 1, 1] 1 // 0
Start = findFirstOccurrence [5, 10, 15, 20, 25, 30] 100 // -1
//Start = findFirstOccurrence [7, 14, 21, 28, 35, 42, 49] 7 // 0
//Start = findFirstOccurrence [] 8 // -1
