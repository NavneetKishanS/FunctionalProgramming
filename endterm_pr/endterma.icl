module endterma
import StdEnv
/* Functional Programming endterm A, 2023. Dec 14.
-- This solution was submitted and prepared by <Aryan Shams Ansari, GTPHX8>
-- for the Functional Programming course.
-- I declare that this solution is my own work.
-- I have not copied or used third-party solutions.
-- I have not passed my solution to my classmates, neither made it public. */
/*---------------*/
/* 1. 
Write a function that takes a string and an integer n 
and returns a new string with the nth element appeneded at 
the end of the string n times. If the index does not exist, 
write "wrong Index".
Eg. Input: "apple" 4, 4th  element -> l, output: "applellll"
*/

fend :: String Int -> String
fend str n 
| size str < n = abort "wrong index"
= str +++ toString[e \\ e <-: str & i <- [1..n] | n == i ]
//Start = fend "apple" 4 // "applellll"
//Start = fend "pear" 5 // "wrong index"
//Start = fend "pear" 2 // "pearee"
/*---------------*/
/* 2.
Write a function that sorts a list of arrays based on the 
length of each array, while preserving the order of elements 
within each array and maintaining the original order 
of arrays that have equal length.
*/

//sortLength :: [{a}] -> [{a}]

//Start = sortLength [{2,2,2},{8,8},{4,4},{3,3,3,3},{2}] 
// [{2},{8,8},{4,4},{2,2,2},{3,3,3,3}]
//Start = sortLength [{5},{3,3,3},{2,4},{1,1,1,1}] 
// [{5},{2,4},{3,3,3},{1,1,1,1}]
//Start = sortLength [] // []
/*---------------*/
/* 3.
Given two strings, check if they are anagrams of each other.
An anagram is a word or phrase formed by rearranging the 
letters of another.
*/
isAnagram :: String String -> Bool   
isAnagram str1 str2 = sum[1 \\ a <-: str1 , b <-: str2 | a == b ] == size str1
//Start = isAnagram "listen" "silent" // True
//Start = isAnagram "hello" "llohe" // True
//Start = isAnagram "rail safety" "fairy tales" // True
//Start = isAnagram "hi" "ihe" // False
/*---------------*/
/* 4.
You are given two arrays, word_arr and len_arr. Each element in 
word_arr is a string, and each element in len_arr is a positive 
integer representing the length of the corresponding string in 
word_arr. Your task is to return an array containing only those 
strings from word_arr whose lengths match the values specified in 
len_arr. Assume that the lengths of both arrays are identical.
*/
str_lengths :: {String} {Int} -> {String}
str_lengths arrStr arrLength = {str \\ str <-: arrStr & len <-: arrLength | size str == len }
//Start = str_lengths {"hello", "world", "clean", "is", "awesome"} {5, 5, 5, 2, 7} 
// {"hello","world","clean","is","awesome"}
//Start = str_lengths {"cat", "dog", "elephant", "lion", "tiger"} {5, 5, 5, 2, 7} // {}
//Start = str_lengths {"cat", "dog", "elephant", "lion", "tiger", "zebra"} {3, 3, 3, -1, 5, 100} // {"cat","dog","tiger"}
//Start = str_lengths {} {} // {}
/*---------------*/
/* 5.1 5p
Make 3 new types Day and Date and DateStamp.
Day is enumaration of the values Sun/Mon/Tue/Wed/Thu/Fri/Sat.
Date is a type synonym for a triple tuple of the type 
(Int,String,Int) having day, month and year.
DateStamp is a type synonym for a tuple of (Day,Date).
*/
:: Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
:: Date :== (Int, String , Int)
:: DateStamp :== (Day, Date)
 
//write the 3 types here

d1 :: Date
d1 = (2,"Jan",2024)
d2 :: Date
d2 = (10,"Aug",2023)
d3 :: Date
d3 = (8, "Apr",2023)
ds1 :: DateStamp
ds1 = (Sun,d1)
ds2 :: DateStamp
ds2 = (Tue,d1)
ds3 :: DateStamp
ds3 = (Sun,d2)
ds4 :: DateStamp
ds4 = (Mon,d3)

//Start = ds2 // (Tue,(2,"Jan",2024))
//Start = d2 // (10,"Aug",2023)
/* 5.2 5p
Write a function that takes a DateStamp and checks if the 
datestamp refers to the first sunday of the month. Assume 
that first sunday can be confirmed if the day is 
Sunday and the day number is less than 7.
*/
instance == Day
where
	(==) Sun Sun = True
	(==) Mon Mon = True
	(==) Tue Tue = True
	(==) Wed Wed = True
	(==) Thu Thu = True
	(==) Fri Fri = True
	(==) Sat Sat = True
	(==) _ _ = False
	
firstSunday :: DateStamp -> Bool
firstSunday d = fst d == Sun && fst3 (snd d) < 7

//Start = firstSunday ds1 // True
//Start = firstSunday ds2 // False
//Start = firstSunday ds3 // False
/*---------------*/
/* 6.
Write a function that takes an array of DateStamps and 
sorts them based on the Day (for same days any order is good).
*/
instance < Day
where
	(<) Sat Sun = True
	(<) Sat Mon = True
	(<) Sat Tue = True
	(<) Sat Wed = True
	(<) Sat Thu = True
	(<) Sat Fri = True
	
	(<) Sun Mon = True
	(<) Sun Tue = True
	(<) Sun Wed = True
	(<) Sun Thu = True
	(<) Sun Fri = True
	
	(<) Mon Tue = True
	(<) Mon Wed = True
	(<) Mon Thu = True
	(<) Mon Fri = True
	
	(<) Tue Wed = True
	(<) Tue Thu = True
	(<) Tue Fri = True	
	
	(<) Wed Thu = True
	(<) Wed Fri = True

	(<) Thu Fri = True
	(<) _ _ = False
	
listToArr :: [a] -> {a}
listToArr list = { e \\ e <- list }
sortDS :: {DateStamp} -> {DateStamp}
sortDS d = listToArr(sort[(a,b) \\ (a,b) <-: d])

//Start = sortDS {ds1,ds2} // {(Sun,(2,"Jan",2024)),(Tue,(2,"Jan",2024))}
//Start = sortDS {ds1,ds2,ds3,ds4} 
// {(Sun,(2,"Jan",2024)),(Sun,(10,"Aug",2023)),(Mon,(8,"Apr",2023)),(Tue,(2,"Jan",2024))}
/*---------------*/
:: University = {uniName::String,students::[Student],teachers::[Teacher]}
:: Teacher = {name::String,subject::String}
:: Student = {studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}
ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BMI::University
BMI={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}
Peter::Teacher
Peter={name="Peter",subject="Functional"}
Viktor::Teacher
Viktor={name="Viktor",subject="Math"}
Mary::Teacher
Mary={name="Mary",subject="OOP"}
John::Teacher
John={name="John",subject="Functional"}
Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}
/*---------------*/
/* 7.
Given the above records, write a function that takes 
an array of Universities and returns a list of those 
subjects that are taught by the favourite Teachers 
of all students that have a gpa more than 4.6,
there should be no duplicates.
*/
arrToList :: {a} -> [a]
arrToList arr = [ e \\ e <-: arr ]
//bestSub :: {University} -> [String]
//bestSub unis = [toString[w.favoriteTeacher.subject \\ w <- e.students] \\ e <-: unis]
//Start = bestSub {ELTE} // ["Functional"]
//Start = bestSub {ELTE,BMI,EmptyUni}// ["Functional", "Math"]
/*---------------*/
/* 8.
Student of the year needs to be awarded to the 
student who is the youngest and has the best gpa.
Write a function that takes a university and returns 
the (name,age,gpa) pair of the awarded student
*/
youngestName :: [Student] -> String
youngestName students = minList[ e.studentName \\ e <- students | youngestAge students == e.age]
youngestAge :: [Student] -> Int
youngestAge students = minList[ e.age \\ e <- students ]
youngestGPA :: [Student] -> Int
youngestGPA students = maxList[ avg(e.grades) \\ e <- students | youngestAge == e.age ]
award :: University -> (String,Int,Real)
award uni = (youngestName uni.students, youngestAge uni.students,  youngestGPA)
//Start = award ELTE // ("Marko",19,4.25)
//Start = award BMI // ("Ana",18,5)
/*---------------*/
:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf
          
atree = (Node 2 (Node 3 (Node 1 Leaf Leaf) Leaf) Leaf)
ctree = Node 1 (Node 2 (Node 8 Leaf Leaf)(Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf)(Node 2 Leaf Leaf))
/* 9.
Write a function that takes a tree of type Int and 
replaces the Leaf whose parent has an even value
with the (Node 1 Leaf Leaf).
*/
extractNodeInt :: (Tree Int) -> Int
extractNodeInt (Node x le ri) = x 
repL :: (Tree Int) -> (Tree Int)
repL Leaf = Leaf
repL (Node x le ri) =(Node x (repL le) (repL ri))
repL (Node x Leaf ri) 
| isEven x = (Node 1 Leaf (repL ri))
repL (Node x le Leaf) 
| isEven x = (Node 1 (repL le) Leaf)
repL (Node x Leaf Leaf) 
| isEven x = (Node 1 Leaf Leaf)
//Start = repL atree // (Node 2 (Node 3 (Node 1 Leaf Leaf) Leaf) (Node 1 Leaf Leaf))
//Start = repL ctree 
//(Node 1 (Node 2 (Node 8 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) (Node 9 (Node 4 (Node 16 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) (Node 1 Leaf Leaf)) Leaf)) (Node 7 (Node 3 Leaf Leaf) (Node 2 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf))))
/*---------------*/
/* 10.
Given a tree structure, write a function transformTree that 
takes a tree and transforms it based on the following rules:
a. Swap the left and right subtrees of every node.
b. Multiply the values of each node by the depth of 
the node in the tree.
example:
             10              level 1
            / \
           5   15            level 2
          / \  / \
         3   7 12 18         level 3
        / \
       1   9 level 4
       
transformedTree:
             10              level 1
            / \
           30   10           level 2
          / \   / \
         54  36 21  9        level 3
           / \
          36   4 level 4
*/
tree1 = Node 10 (Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf))
tree2 = Node 12 (Node 8 (Node 4 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)) (Node 10 Leaf (Node 15 Leaf (Node 14 Leaf Leaf)))
depth :: (Tree a) -> Int
depth Leaf = 0
depth (Node x le ri) = (max (depth le)(depth ri)) + 1

//Start = depth tree1
transTree :: (Tree Int) -> (Tree Int)
transTree Leaf = Leaf
transTree (Node x le ri) = (Node ((depth (Node x le ri)) * x) ri le)

Start = transTree tree1 
//(Node 10 (Node 30 (Node 54 Leaf Leaf) (Node 36 Leaf Leaf)) (Node 10 (Node 21 Leaf Leaf) (Node 9 (Node 36 Leaf Leaf) (Node 4 Leaf Leaf))))
//Start = transTree tree2
//(Node 12 (Node 20 (Node 45 (Node 56 Leaf Leaf) Leaf) Leaf) (Node 16 (Node 21 Leaf (Node 24 Leaf Leaf)) (Node 12 Leaf Leaf)))
/*---------------*/
/* 11.
Given a binary search tree and an int, return a list 
of all the elements along the path from the root node 
to the int we are looking for. Assume that the tree 
contains unique elements. If the int does not exist 
in the tree return an empty list. */
bst1 = (Node 1 Leaf (Node 20 (Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf (Node 12 (Node 5 Leaf Leaf) Leaf))) (Node 45 (Node 34 (Node 22 Leaf Leaf) Leaf) (Node 112 (Node 53 Leaf Leaf) Leaf))))
bst2 = (Node 1 Leaf (Node 20 (Node 7 Leaf (Node 12 (Node 11 (Node 9 Leaf Leaf) Leaf) Leaf)) Leaf))
bst3 = (Node 1 Leaf (Node 20 (Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf (Node 10 (Node 8 Leaf Leaf) Leaf))) (Node 45 (Node 34 (Node 22 Leaf Leaf) Leaf) (Node 112 (Node 53 Leaf Leaf) Leaf))))
inOrder :: (Tree Int) -> [Int]
inOrder Leaf = []
inOrder (Node x le ri) = [x] ++ inOrder le  ++ inOrder ri 
pathTo :: (Tree Int) Int -> [Int]
pathTo tree x
| isEmpty(inOrder tree) = []
| not(isMember x (inOrder tree)) = []
= takeWhile (\e = e <> x) (inOrder tree) ++ [x]
//Start = pathTo bst1 45 //[1,20,45]
//Start = pathTo bst2 9 // [1,20,7,12,11,9]
//Start = pathTo bst3 53 // [1,20,45,112,53]
//Start = pathTo bst3 54 // []
/*---------------*/
/* 12.
Given class 'Op' for any type a with the '--' binary operator, 
which takes 2 arrays of any type a and returns an array of 
any type a elements. Write an instance to define '--' operator 
for arrays of Char type, which is the difference of its given 
arguments' union and intersection, in sorted order without duplicates. 
*/
class Op a 
where
    (--) :: {a}  {a} -> {a}
   
//instance -- {a}
//where
//	(--) a b = listToArr[a\\ q <-: a & w <-: b | (isMember w a)]
  
//Start={'a','b'} -- {'b','c'} //{'a','c'}
//Start={'h','e','l','l','o'} -- {'w','o','r','l','d'}
//{'d','e','h','r','w'}
//Start={'h','e','l','l','o','g','o','o','d'} -- {'m','o','r','n','i','n','g'} 
//{'d','e','h','i','l','m','n','r'}
/*---------------*/


