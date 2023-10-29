module HW1
import StdEnv

//Submitted by: SRINIVASAN NAVNEET KISHAN

/*
Please write your neptun code here: JZD5GY
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
Write a function named distance3D that calculates the Euclidean distance
between two points in a 3-dimensional space.

Six Real numbers representing the coordinates of the two points in the 3D space: 

Hint: https://www.varsitytutors.com/hotmath/hotmath_help/topics/distance-formula-in-3d
*/

distance3D :: Real Real Real Real Real Real -> Real
distance3D a b c x y z = sqrt(	((a-x)^2.0)	+	((b-y)^2.0)	+	((c-z)^2.0)	)

//Start = distance3D 0.0 0.0 0.0 1.0 1.0 1.0 // 1.73205080756888
//Start = distance3D 1.0 2.0 3.0 4.0 5.0 6.0 // 5.19615242270663
//Start = distance3D -1.0 -1.0 -1.0 1.0 1.0 1.0 // 3.46410161513775
//Start = distance3D 0.0 0.0 0.0 0.0 0.0 0.0 // 0
//Start = distance3D 1.1 2.2 3.3 4.4 5.5 6.6 // 5.7157676649773

//CUSTOM TEST CASE	:
//Start = distance3D 1.0 1.0 -1.0 0.0 -2.0 3.0 //5.09901951359278


/*
Problem2:
Write a function called isSorted that takes five integer inputs: a, b, c, d, and e.
Your task is to determine whether these numbers are in sorted order, in increasing sequence.
The function should return True if the numbers are sorted, and False otherwise.

To clarify, the numbers should be checked in ascending order,
meaning that a should be less than or equal to b, b should be less than or equal to c,
c should be less than or equal to d, and d should be less than or equal to e.
If these conditions are met, your function should return True. Otherwise, it should return False.

Your function should correctly handle cases where some of the input numbers are equal.
In such cases, it should still return True if the numbers are sorted, even if some of them are equal.
For example, if the inputs are (3, 3, 4, 4, 5),
your function should return True because the numbers are in ascending order, even though some are equal
*/

isSorted :: Int Int Int Int Int -> Bool
isSorted a b c d e = (a<=b) && (b<=c) && (c<=d) && (d<=e)

//Start = isSorted 1 2 3 4 5 // True
//Start = isSorted 10 5 3 2 1 // False
//Start = isSorted 0 0 0 0 0 // True
//Start = isSorted 100 200 300 400 500 // True
//Start = isSorted 5 5 5 5 6 // True

//CUSTOM TEST CASE :
//Start = isSorted 3 3 4 4 5 //true 