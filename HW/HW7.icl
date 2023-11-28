module HW7
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
Develop a function named mostCommonChar to identify the most frequently occurring character in a given string. If multiple characters occur with the same highest frequency, the function should return the one that appears first in the string.

The function should count the occurrences of each character in the string.
It should identify the character(s) with the highest frequency of occurrence.
In case of a tie, the first character (based on the order of appearance in the string) among those tied for the most occurrences should be returned.
The input string is guaranteed to be non-empty, so the function always returns a character.
*/

chars :: String -> [Char]
chars str = [e \\ e<-: str]

count :: Char [Char] -> Int
count n ns = length [x \\ x <- ns | x == n]

countOcc :: [Char] -> [Int]
countOcc [] = []
countOcc ls = [count e ls \\ e <- ls]

findIndex :: Int Int [Int] -> Int
findIndex n i [x:xs]
| not(isMember n [x:xs]) = -1
| n == x = i
= findIndex n (i+1) (xs)

mostCommonChar :: String -> Char
mostCommonChar "" = ' '
mostCommonChar str = (chars(str)) !! (findIndex a 0 (countOcc (chars(str)) ))
where a = (last (sort[count a (chars(str)) \\ a<-(chars(str))]))

//Start = mostCommonChar  "abracadabra" // 'a'
//Start = mostCommonChar  "!@#$%^&*()!" //  '!'
//Start = mostCommonChar  "This is a test sentence." //  's'
//Start = mostCommonChar  "123a321" // '1'
//Start = mostCommonChar "hello world, how are you?" // 'o'

/*
Problem2:
Step 1: Define a Record Type for a Person
Create a record type named Person with the following fields:

name: A String representing the person's name.
age: An Int representing the person's age.
email: A String representing the person's email address.

Step 2: Write a Filtering Function
Develop a function named filterGmailAdults that performs the following:

Accepts a list of records of type Person.
Processing: The function filters out persons from the list based on two criteria:
The person's age should be over 20 years.
The person's email address should end with "@gmail.com".
Output: Returns a list of Strings, each representing the name of a person who meets both criteria.

Expected Behavior:
Persons under the age of 20 should be excluded, regardless of their email address.
Persons with email addresses not ending with "@gmail.com" should be excluded, regardless of their age.
The function should return only the names of persons who satisfy both conditions.

If no person in the list meets the criteria, the function should return an empty list.
*/
 
// Person record type

::Person = { name :: String
			, age :: Int
			, email :: String}

p1 :: Person
p1 = {name="Alice", age= 25, email="alice@gmail.com"}
p2:: Person
p2 = {name="Frank", age= 17, email="frank@inf.elte.hu"}
p3 :: Person
p3 = {name="Grace", age= 30, email="grace@gmail.com"}
p4 :: Person
p4 = {name="Dave", age= 22, email="dave@yahoo.com"}

filterAge :: [Person] -> [Person]
filterAge [] = []
filterAge [p:ps ]
| p.age > 20 = [p] ++ filterAge ps
= filterAge ps

filterMail :: Person -> Bool
filterMail p = ("gmail.com" == toString (tl(map toChar (dropWhile (\x = x <> 64) (map (toInt)(chars(p.email)))))))

personName :: Person -> String
personName p = p.name

filterGmailAdults :: [Person] -> [String]
filterGmailAdults [] = []
filterGmailAdults ls = map personName (filter (filterMail) (filterAge ls))

//Start = filterGmailAdults [p1, p2, p3, p4] // ["Alice","Grace"]
//Start = filterGmailAdults [p1, p2, p4] // ["Alice"]
//Start = filterGmailAdults [] //  []