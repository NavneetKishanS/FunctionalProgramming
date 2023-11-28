module HW6
import StdEnv

/*
Please write your name and neptun code here: NAVNEET KISHAN SRINIVASAN - JZD5GY
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
Write a function kthLargestAndSmallest :: [Int] Int -> (Int, Int) that takes a list of integers 
and an integer k, and returns a tuple where the first element is the kth largest number and the second element 
is the kth smallest number from the given list.

If the list has fewer than k elements or k is less than 1, 
return a tuple with both elements as -1.
*/

kthLargestAndSmallest :: [Int] Int -> (Int, Int)
kthLargestAndSmallest [] x = (-1,-1)
kthLargestAndSmallest ls k 
| k > length ls = (-1,-1)
= (((reverse(sort ls))!!(k-1)) , ((sort ls)!!(k-1)) )


//Start = kthLargestAndSmallest [10, 20, 30, 40, 50] 2 // (40, 20)
//Start = kthLargestAndSmallest [5, 3, 8, 1, 4] 3      // (4, 4)
//Start = kthLargestAndSmallest [7, 7, 7, 7, 7] 1      // (7, 7)
//Start = kthLargestAndSmallest [25, 36, 12] 4         // (-1, -1)

/*
Problem2:
Given a list of lists of real numbers, for every sublist   find the item in the  sublist which is closest
to the average of the sublist.
e.g [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] ->  avg of [1.3, 5.2, 7.7, -2.3, 23.45]  is 7.07 so the closest value from the list is  7.7
similarly, avg of [3.0,8.4] is 5.7  so the closest value from the list is  3.0
*/

avgCalc :: [Real] -> Real
avgCalc [] = 0.0
avgCalc ls = hd (sort[ abs(x - y) \\ x<-ls] ) + y
where y = (avg ls)

closestToAvg :: [[Real]] -> [Real]
closestToAvg [[]] = []
closestToAvg ls = map avgCalc ls

//Start = closestToAvg [[1.3, 5.2, 7.7, -2.3, 23.45] , [3.0,8.4] ] //  [7.7,8.4]
//Start = closestToAvg [[2.4 ,4.5 ,6.7 ,6.6 ,7.7] , [5.6 , 6.8 ,4.8 , 4.1] , [5.5,5.1] , [5.0] , [7.8] ] // [6.6,5.6,5.5,5,7.8]
//Start = closestToAvg [[1.3]] // [1.3]