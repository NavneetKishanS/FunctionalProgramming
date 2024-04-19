module PT1gr13
import StdEnv


/* Your neptun code :        */

/*1- Write a function findBMICategory that takes a Real representing the BMI of a person and returns a String representing the BMI category of the person based on the given table:
-------------------------------------
|     BMI      |     Category      |
-------------------------------------
| 18.5 or less | Underweight       |
-------------------------------------
| 18.6 - 24.9  | Normal weight     |
-------------------------------------
| 25.0 - 29.9  | Overweight        |
-------------------------------------
|  30 or more  | Obesity           |
-------------------------------------


*/

findBMICategory :: Real -> String
findBMICategory x
| (0.0 <= x) && (x <= 18.5) = "Underweight"
| (18.6 <= x) && (x <= 24.9) = "Normal weight"
| (25.0 <= x) && (x <= 29.9) = "Overweight"
| (30.0 <= x) = "Obesity"
= "Invalid BMI"

//Start = findBMICategory -5.0 // "Invalid BMI"
//Start = findBMICategory 18.0 // "Underweight"
//Start = findBMICategory 20.5 // "Normal weight"
//Start = findBMICategory 27.0 // "Overweight"
//Start = findBMICategory 30.5 // "Obesity"
