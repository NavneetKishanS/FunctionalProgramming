module PT4gr13
import StdEnv

/*
Please write your name and neptun code here: NAVNEET KISHAN SRINIVASAN - JZD5GY

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
Task:
Given a list of pairs (tuples) containing an integer and a count, return a list of integers where each integer in the original list is followed by a number of zeros specified by its count.

Example:
For the tuple (5, 2), the resulting list will be [5, 0, 0].
*/


expandList :: [(Int, Int)] -> [Int]
expandList [] = []
expandList [(a,b) :ls] = [a] ++ repeatn b 0 ++ expandList ls


//Test
//Start = expandList [(5, 2), (3, 3)] //[5, 0, 0, 3, 0, 0, 0]
//Start = expandList [(4, 1), (2, 4)] //[4, 0, 2, 0, 0, 0, 0]
//Start = expandList [(7, 0), (8, 2)] //[7, 8, 0, 0]