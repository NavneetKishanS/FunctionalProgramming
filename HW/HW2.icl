module HW2
import StdEnv

/*

Please write your neptun code here: 
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
Write a recursive function gcd that takes two positive integers, a and b,
as input and calculates their GCD using the Euclidean algorithm.

The GCD of two positive integers is the largest positive integer
that divides both numbers without leaving a remainder.

Your function should use recursion to compute the GCD.
*/

gcd :: Int Int -> Int
gcd a b
#c = (a rem b)
| (b<0) || (a<0) = abort "Both numbers must be positive."
| (b ==0) = a
= gcd b c

//Start = gcd 8 12 // 4
//Start = gcd 27 81 // 27
//Start = gcd 0 7 // 7
//Start = gcd -17 19 // Both numbers must be positive.
//Start = gcd -5000000 7000000 // Both numbers must be positive.

/*
Problem2:
Write a recursive function fibonacci that takes a positive integer n
as input and calculates the nth number in the Fibonacci sequence.

Your function should use recursion to compute the factorial.
*/

fibonacci :: Int -> Int
fibonacci n
| (n<0) = abort "Input must be a non-negative integer."
| (n==0) || (n==1) = n
= fibonacci (n-1) + fibonacci (n-2)

//Start = fibonacci 0 // 0
//Start = fibonacci 1 // 1
//Start = fibonacci 5 // 5
//Start = fibonacci 8 // 21
//Start = fibonacci 12 // 144
//Start = fibonacci -48 // Input must be a non-negative integer.
//Start = fibonacci -958654 // 	Custom Test Case
