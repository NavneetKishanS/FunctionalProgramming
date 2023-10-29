module second

import StdEnv

myabs :: Int -> Int
myabs x
| x < 0 = ~x //tilde unary operator
| x == 0 = 0
| x > 0 = x

//Start = myabs (-1)

maxval :: Int Int -> Int
maxval x y
| x > y = x
= y

//Start = maxval 5 6

add100 :: Int -> Int
add100 x = x + 100

//Start = add100 4

triple :: Int -> Int
triple x = 3*x

//Start = triple 5

fact :: Int -> Int

fact x
  | x == 0 = 1
  | x == 1 = 1
  | otherwise = x * fact (x - 1)

//Start = fact 5

oddeve :: Int -> Bool
oddeve x
= (x rem 2) == 0 

//Start = oddeve 4

hw :: String String -> String
hw a b = a +++ b

//Start = hw "Hello" "World"

cube :: Int -> Int
cube x = x*x*x

//Start = cube 3

ismult10 :: Int -> Bool
ismult10 x
= ((x rem 10) == 0)

//Start = ismult10 30

issum :: Int Int Int -> Bool
issum a b c
| (a == b + c) = True
| (b == a + c) = True
| (c == b + a) = True
= False

//Start = issum 8 4 4

samerem :: Int Int Int -> Bool
samerem a b c = (a rem c == b rem c)

//Start = samerem 12 4 4

check :: Int Int Bool -> Bool
check a b x = (a rem 2 ==0) && (b rem 13 ==0) && x == True

Start = check 4 26 True