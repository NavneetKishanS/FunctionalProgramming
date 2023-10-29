
module first
import StdEnv
import Math.Geometry


//Start = "Hello World"


//______________________________________________________-
//random stuff
//my_remainder :: Int Int -> Int
//my_remainder x y
//    | x < y = x
//    | otherwise = my_remainder (x - y) y
//Start = my_remainder 20 5 // 0
//______________________________________________________-
 
f :: Int -> Int
f x = x^2

//Start = f 5


//______________NOTES____________________________


//______________________________________________________________________________________________________
//f :: InputType -> OutputType//Function Definition
//f x = x^2 //function operation '=' here is return
// Start = f 5 //main function *****make sure that the input-type of function is same as defined***
//
//when using the same icl file make sure to give different names to functions
//______________________________________________________________________________________________________

f1 :: Int -> Int
f1 x = x/2

//Start = f1 5

h3y :: Real Real Real -> Int
h3y x y z= toInt(x + 2.0*y*z)

//Start = h3y 2.0 8.0 0.5


//______________NOTES____________________________


//__________________________________________________________________
// +++ for concatenation
//__________________________________________________________________


//Task
task :: Int Real -> Real
task x y = abs(toReal(x)/y)

//Start = task 2 0.5

realDiv :: Real -> Real
realDiv x = x / 2.0

//Start = realDiv 5.0

h3yr :: Real Real Real -> Real
h3yr x y z = x + 2.0*y*z

//Start = h3yr 1.0 2.0 3.3 

b :: Bool -> Bool
b x = not x

//Start = b True //False

b2 :: Bool -> Bool
b2 x = not (not x)

//Start = b2 True // True

b3 :: Bool -> Bool
b3 x = (not (not x)) == x

//Start = b3 True //True


st :: String String String -> String
st a b c = a +++ b +++ c

//Start = st "Clean" "FP" "Programming"

c :: Char -> Char
c x = x

//Start = c '%' //%

//Start = toInt(abs -4.3587) // 4 built-in abs function

div :: Real -> Real
div x = x / 2.0

//Start = (div 5.0, div 5.0, div 1.0, div 10.0)

divbr :: Int Int -> Real
divbr x y = toReal(x/y)

//Start = divbr 5 3 // 1


divbr2 :: Int Int -> Real
divbr2 x y = (toReal x)/(toReal y)

//Start = divbr2 5 3 //1.666666667


isEven :: Int -> Bool
isEven a = (a rem 2 == 0) 

//Start = isEven 3 //False

isEvenWr :: Int -> Bool
isEvenWr a
| (a rem 2 == 0) = True
| otherwise = False

//Start = isEvenWr 5 // False

isEven2 :: Int -> Bool
isEven2 a = (a/2)*2 == a

//Start = isEven2 23

granma :: Real Real Real Real Real -> Real
granma a b c d e = a*500.0 + b*800.0 + c*150.5 + d*4000.0 + e*700.0

//Start = granma 5.0 7.0 10.0 2.0 1.0 //18305

//Mt Everest K2 Problem

ME = 8848
K2 = 8611

dist :: Int -> Int
dist x = (ME-K2) + x 

//Start = dist 1000 //1237

//Circle Area
//Pi = 3.142
cir :: Real -> Real
cir x = pi*x*x

//Start = cir 2.0

//quadratic num
qa :: Int Int -> Int
qa x n = x^n

//Start = qa 2 4 //16

//Quadratic Eqn
//qeq :: Real Real Real -> Real
//qeq a b c = 


//app :: Real -> Real
//org = 200.0
//org += 0.1*org
//app x = org ^x

//Start = app 7.0

