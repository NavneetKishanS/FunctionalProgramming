module endterm1
import StdEnv

/*---------------------------------------------------------------
-- Functional Programming end-term

-- This solution was submitted and prepared by
-- <NAME, NEPTUN> for the end-term programming assignment of
-- the Functional Programming course.

-- I declare that this solution is my own work.

-- I have not copied or used third-party solutions.

-- I have not passed my solution to my classmates, neither made it public.

-- Students' regulation of Eotvos Lorand University (ELTE Regulations Vol. II. 74/C.)
-- states that as long as a student presents another student’s work -
-- or at least the significant part of it - as his/her own performance,
-- it will count as a disciplinary fault.

-- The most serious consequence of a disciplinary fault can be dismissal
-- of the student from the University.
*/

// 1.----------------------------
/* Record Student. (10 points)
*
* Create an Algebraic type `StudyLevel` which can have
* 3 values: `BSc`, `MSc`, `PhD`.
* Create a record `Student` which has 3 fields:
* * id - A string id
* * level - A study level with type `StudyLevel`
* * grades - A list of grades, where each grade is an integer
* Write a `getBestBScStudent` function that takes a list of students
* and returns a string ID of the student who has highest average grade
* among BSc students. Assume that the list contains at least one BSc student.
* If multiple students have same maximum average return any of their IDs.
*/

::StudyLevel = BSc | MSc | PhD

::Student = {id::String,level::StudyLevel,grades::[Int]}

//class Level
instance == StudyLevel
where
	(==) BSc BSc = True
	(==) MSc MSc = True
	(==) PhD PhD = True
	(==) _ _ = False
	


st1 = {id="st-1", level=BSc, grades=[3,4,3]}
st2 = {id="st-2", level=MSc, grades=[3,1,3]}
st3 = {id="st-3", level=PhD, grades=[5]}
st4 = {id="st-4", level=BSc, grades=[5,5,4]}
st5 = {id="st-5", level=BSc, grades=[5,5,5,2,4]}

/*
instance == Int
where
	(==) Int Int = True
	(==) _ _ = False*/
	

highGPA::[Student] -> Int //String
highGPA [] = 0//""
highGPA ls = length(sort[avg(x.grades) \\ x<-ls ])

highGPAstudent ::[Student] -> [(String,Int)]
highGPAstudent [] = []
highGPAstudent ls = [(x.id,avg(x.grades)) \\ x<-ls | x.level == BSc]//[(x.id,avg(x.grades)) \\ x<-ls ]
//= [(x.id,avg(x.grades)) \\ x<-ls | highGPA ls == avg(x.grades)]
//highGPA ls == avg(x.grades) &&  

tupleToInt :: [(String,Int)]->[Int]
tupleToInt ls = [snd x \\ x<- ls]

//Start = tupleToInt(highGPAstudent [st1,st2,st3,st4,st5])

/*
highGPA2 ::[(String,Int)] -> String
highGPA2 [] =""
highGPA2 ls = */
//highGPA ls =+ avg(x.grades) && 
//Start = highGPAstudent [st1,st2,st3,st4,st5]
//Start = highGPA [st1,st2,st3]

//getBestBScStudent :: [Student] -> String
//getBestBScStudent [] = ""
//getBestBScStudent ls = [x.id \\ x<-ls]

//Start = getBestBScStudent [st1] // "st-1"
//Start = getBestBScStudent [st1, st4, st5] // "st-4"
//Start = getBestBScStudent [st2, st3, st5] // "st-5"
//Start = getBestBScStudent [st1, st2, st3, st4, st5] // "st-4"
//Start = getBestBScStudent [] // "ERROR"


// 2.----------------------------
/* Arrays. (10 points)
*
* Implement the 'count' function which takes a string and counts the repeated characters

* in the string. Example:
* Input: "thequickbrownfoxjumpsoverthelazydog"
* Output: {(o,4),(e,3),(u,2),(h,2),(r,2),(t,2)}
* Explanation:
o is repeated 4 times in the given string
e is repeated 3 times in the given string
u is repeated 2 times in the given string
h is repeated 2 times in the given string
r is repeated 2 times in the given string
t is repeated 2 times in the given string

*/

charCount :: Char String -> (Char,Int)
charCount ch word = (ch,length[x\\x<-:word | x==ch])

//Start = charCount 'c' "Accccafbgfd"
//Start = {i\\i<-[charCount x "aaabbbcccdddeee"\\x<-(removeDup[ch \\ ch<-:"aaabbbcccdddeee"])]}

count :: String -> {(Char, Int)}
count str = {i\\i<-[charCount x str \\ x<-(removeDup[ch \\ ch <-:str])]|snd i >= 2}

//Start = count "thequickbrownfoxjumpsoverthelazydog" // {('t',2),('h',2),('e',3),('u',2),('r',2),('o',4)}
//Start = count "Helloworld" // {('l',3),('o',2)}
//Start = count "FUNCTIONSLPROGRAMMINGISFUN" // {('F',2),('U',2),('N',4),('I',3),('O',2),('S',2),('R',2),('G',2),('M',2)}
//Start = count "Cleanisamazing" // {('a',3),('n',2),('i',2)}
//Start = count "computerscience" // {('c',3),('e',3)}
//Start = count "" // {}


// 3.----------------------------
/* Instances. (20 points) Note this task has 4 parts each of 5 points!
*
* Write 4 instances for lists of integers
* 1. '*' - Takes 2 lists and multiplies elements
* pairwise. If they have different lengths
* use shortest. Ex.: [1,2,3] * [2, 4] = [2, 8]
* 2. '+' - Takes 2 lists and adds elements
* pairwise. If they have different lengths
* use shortest. Ex.: [1,2,3] + [2, 4] = [3, 6]
* 3. '~' - Takes 1 list and removes those elements that are
* negative or zero and return remaining list.
* Ex.: ~[1, ~2] = [1]
* 4. '-' - Takes 2 lists and computes the "differences" of lists
* Ex.: [1,2,3,4,5] - [2,4] = [1,3,5]
*/

//*
instance * [Int]
where
	(*) a b = [x*y \\ x<-a & y<-b]

//+
instance + [Int]
where
	(+) a b = [x+y \\ x<-a & y<-b]

//~
/*instance ~ [Int]
where
	(~) a = [abs x \\ x<-a]*/

//-
instance - [Int]
where
	(-) a b = [x \\ x<-a | not(isMember x b)]

//Start = [1, 2, -1] * [2, 3, 4] // [2,6,-4]
// Start = [1,2,3] * [2, 4] // [2, 8]
// Start = [1, 2, -1] + [2, 3, 4] // [3, 5, 3]
// Start = [1,-2,3] + [2, 4] // [3, 2]
// Start = ~[1, ~1, 3, ~2, ~3, 4] // [1, 3, 4]
// Start = ~[~1, ~2] // []
// Start = [1..5] - [2,4] // [1, 3, 5]
// Start = [1..10]-[1..8] // [9,10]
// Start = [1..8] - [1..10] // []


// 4.----------------------------
/* Tree height. (10 points)
*
* Given a BinaryTree structure, write a function `getBTHeight`
* that takes a BinaryTree and calculates its height.
* The height of a binary tree is the total number of nodes from
* the root node to the most distant leaf node.
* Ex.: this is just illustration, the exercise has no values in nodes and leaves
* 1
* / \             The height of this tree is 5.
* 2  3           The farthest nodes are 10, 11, 12 and 13
* / \             And the distance from root to any of them
* 4  5          is 5. Other nodes are closer to the root.
*  / \    / \
* 6  7   8 9
* / \           / \
* 10 11 12 13
*/

:: BinaryTree = BTNode BinaryTree BinaryTree | BTLeaf

bt1 = (BTNode BTLeaf (BTNode BTLeaf BTLeaf))
bt2 = (BTNode (BTNode BTLeaf BTLeaf) (BTNode BTLeaf BTLeaf))
bt3 = (BTNode (BTNode bt2 bt1) (BTNode BTLeaf bt2))
bt4 = (BTNode (BTNode bt3 bt1) (BTNode BTLeaf bt2))

getBTHeight :: BinaryTree -> Int
getBTHeight BTLeaf = 1
getBTHeight (BTNode le ri) = (max (getBTHeight le) (getBTHeight ri)) + 1

//Start = getBTHeight BTLeaf // 1
//Start = getBTHeight bt1 // 3
//Start = getBTHeight bt2 // 3
//Start = getBTHeight bt3 // 5
//Start = getBTHeight bt4 // 7


// 5.----------------------------
/*
* Linked List. (20 points) Note this task has 4 parts each of 5 points!
*/

:: LinkedList a = Pointer a (LinkedList a) | Nil

linkedlist1:: (LinkedList String)
linkedlist1 = Pointer "o" (Pointer "l" (Pointer "l" (Pointer "e" (Pointer "H" (Nil)))))

linkedlist2:: (LinkedList String)
linkedlist2 = Pointer "y" (Pointer "e" (Pointer "H" Nil))

// 5.1.----------------------------
/* Insert. (5 points)
*
* Complete the function Insert that takes a linked list and a value,
* and inserts a new node at the end of it with the given value.
*/

Insert :: (LinkedList String) String -> (LinkedList String)
Insert Nil s = Pointer s Nil
Insert (Pointer x next) s = Pointer x (Insert next s)
//Insert Nil str = (Pointer str Nil) 
//Insert (LinkedList) str = Insert (Pointer LinkedList  )

//Start = Insert linkedlist1 "World" // (Pointer "o" (Pointer "l" (Pointer "l" (Pointer "e" (Pointer "H" (Pointer "World" Nil))))))
//Start = Insert linkedlist1 "" // (Pointer "o" (Pointer "l" (Pointer "l" (Pointer "e" (Pointer "H" (Pointer "" Nil))))))
//Start = Insert linkedlist2 "!!" // (Pointer "y" (Pointer "e" (Pointer "H" (Pointer "!!" Nil))))
//Start = Insert Nil "bye-bye" // (Pointer "bye-bye" Nil)

// 5.2.----------------------------
/* Reverse. (5 points)
*
* Complete the function Reverse that takes a linked list,
* and returns a reversed version of it.
*/

/*
reverseHelper :: (LinkedList String) -> (LinkedList String)
reverseHelper Nil reversed = reversed
reverseHelper (Pointer val rest) reversed = reverseHelper rest (Pointer val reversed)
Start = reverseHelper linkedlist1
*/

Reverse :: (LinkedList String) -> (LinkedList String)
Reverse list = reverseHelper list Nil

where
    reverseHelper :: (LinkedList String) (LinkedList String) ->(LinkedList String)
    reverseHelper Nil reversed = reversed
    reverseHelper (Pointer val rest) reversed = reverseHelper rest (Pointer val reversed)

//Reverse :: (LinkedList String) -> (LinkedList String)
//Reverse Nil = Nil
//Reverse (Pointer x next) = next (Reverse Pointer x) 

//Start = Reverse linkedlist1 //(Pointer "H" (Pointer "e" (Pointer "l" (Pointer "l" (Pointer "o" Nil)))))
//Start = Reverse (Insert linkedlist1 "World") //(Pointer "World" (Pointer "H" (Pointer "e" (Pointer "l" (Pointer "l" (Pointer "o" Nil))))))
//Start = Reverse linkedlist2 // (Pointer "H" (Pointer "e" (Pointer "y" Nil)))
//Start = Reverse Nil // Nil

// 5.3.----------------------------
/* Delete. (5 points)
*
* Complete the function delete that takes a linked list and a value,
* and removes the first occurrence of the given value if it exists.
*/

//delete ::(LinkedList String) String -> (LinkedList String)

delete :: (LinkedList String) String -> (LinkedList String)
delete Nil _ = Nil  // Base case: Empty list, nothing to delete
delete (Pointer val rest) target
| val == target = rest  // If the target is found at the head, return the rest of the list
| otherwise = Pointer val (delete rest target)  // If the target is not found at the head, proceed recursively


//Start = delete linkedlist1 "h" //(Pointer "o" (Pointer "l" (Pointer "l" (Pointer "e" (Pointer "H" Nil)))))
//Start = delete linkedlist1 "H" // (Pointer "o" (Pointer "l" (Pointer "l" (Pointer "e" Nil))))
//Start = delete linkedlist2 "y" // (Pointer "e" (Pointer "H" Nil))
//Start = delete Nil "Hye" // Nil

// 5.4.----------------------------
/* Concatenation. (5 points)
*
* Complete the function concat that takes two linked list and
* concatenates the second to the end of first.
*/

concat ::(LinkedList String) (LinkedList String) -> (LinkedList String)
concat Nil list2 = list2  // If the first list is empty, return the second list
concat (Pointer val rest) list2 = Pointer val (concat rest list2)

//Start = concat linkedlist1 linkedlist2 // (Pointer "o" (Pointer "l" (Pointer "l" (Pointer "e" (Pointer "H" (Pointer "y" (Pointer "e" (Pointer "H" Nil))))))))
//Start = concat linkedlist2 linkedlist2 // (Pointer "y" (Pointer "e" (Pointer "H" (Pointer "y" (Pointer "e" (Pointer "H" Nil))))))
//Start = concat Nil linkedlist2 // (Pointer "y" (Pointer "e" (Pointer "H" Nil)))
//Start = concat linkedlist2 Nil // (Pointer "y" (Pointer "e" (Pointer "H" Nil)))
//Start = concat Nil Nil // Nil


// 6.----------------------------
/* Fake Accounts. (10 points)
*
* Given an array of BasicPersonAccount, ban the fake accounts
* (remove them from the array, only the first original should remain),
* and dequeue the original accounts of the fakes
* (inQueue should be set to False if they had fakes).
* To find a fake account, check the names and ips of the accounts.
* i.e: fakePer1 is a fake account of per1 since both of them have
* the same name and ip and but they are different in age,
* while posssibleFake is not a fake account of per3.
*/

:: BasicPersonAccount = {name:: String, age::Int, inQueue::Bool, ip::Int}

per1 :: BasicPersonAccount
per1 = {name = "A", age=45, inQueue=True, ip = 100025 }

per2 :: BasicPersonAccount
per2 = {name = "B", age=22, inQueue=True, ip = 755542}

per3 :: BasicPersonAccount
per3 = {name = "C", age=18, inQueue=True, ip = 155200}

fakePer1 :: BasicPersonAccount
fakePer1 = {name = "A", age=12, inQueue=True, ip = 100025}

posssibleFake :: BasicPersonAccount
posssibleFake = {name = "C", age=18, inQueue=True, ip = 12205}

//findFakes :: {BasicPersonAccount} -> {BasicPersonAccount}

//Start = findFakes {per1, fakePer1, per2, per3, fakePer1, posssibleFake}
//{(BasicPersonAccount "A" 45 False 100025),(BasicPersonAccount "B" 22 True 755542),(BasicPersonAccount "C" 18 True 155200),(BasicPersonAccount "C" 18 True 12205)}
//Start = findFakes {per1, per1, per1} // {(BasicPersonAccount "A" 45 True 100025)}
//Start = findFakes {per3, posssibleFake} // {(BasicPersonAccount "C" 18 True 155200),(BasicPersonAccount "C" 18 True 12205)}
//Start = findFakes {} // {}


// 7.----------------------------
/* Matrices. (10 points)
*
* Given an array of different Matrices, return the matrix with the largest diagonal sum.
* Mat1 = [[1,2,3],[1,1,1], [3,3,3]] Mat1 diagonal sum is 1 + 1 + 3 = 3
* Mat2 = [[3,3,3],[0,0,0], [1,1,1]] Mat2 diagonal sum is 3 + 0 + 1 = 4
* Therefore, Mat1 is the largest between these two matrices.
* The elements of diagonal are on the i-th row and in i-th column.
*/

Mat1 = [[1,2,3], [1,1,1], [3,3,3]]
Mat2 = [[3,3,3], [0,0,0], [1,1,1]]
Mat3 = [[3,3,3,3], [3,3,3,3], [3,3,3,3], [3,3,3,3]]
Mat4 = [[3,3,3,3], [3,(-3),3,3], [3,3,(-3),3], [3,3,10,3]]

largestMat :: {[[Int]]} -> [[Int]]
largestMat arr = [a \\ a<-: arr | (diagSum a) == (last(sort[diagSum i \\ i<-:arr]))]!!0

diagSum :: [[Int]] -> Int
diagSum [[]] = 0
diagSum ls = sum[((ls!!i)!!i) \\ i<-[0..(length(ls)-1)] ]

//Start = diagSum Mat1

//Start = largestMat {Mat1, Mat2} // [[1,2,3],[1,1,1],[3,3,3]]
//Start = largestMat {Mat1, Mat3} // [[3,3,3,3],[3,3,3,3],[3,3,3,3],[3,3,3,3]]
//Start = largestMat {Mat2, Mat3} // [[3,3,3,3],[3,3,3,3],[3,3,3,3],[3,3,3,3]]
//Start = largestMat {Mat1, Mat2, Mat3, Mat4} // [[3,3,3,3],[3,3,3,3],[3,3,3,3],[3,3,3,3]]
//Start = largestMat {Mat1, Mat2, Mat4} // [[1,2,3],[1,1,1],[3,3,3]]


// 8.----------------------------
/* Linked List Representation. (10 points)
*
* The type MyList represents a list data structure, every element holds a value and a child.
* Create a toString instance for this type that converts it to a string that looks like a list.
* Elem 4 (Elem 3 (Elem 2 (Elem 1 (Empty) ) ) ) should output "[4,3,2,1]"
*/

:: MyList a = Elem a (MyList a) | Empty


list1 :: (MyList Int)
list1 = Elem 4 (Elem 3 (Elem 2 (Elem 1 (Empty) ) ) )

list2 :: (MyList Int)
list2 = Elem 2 (Elem 6 (Elem 6 (Elem 8 (list1) ) ) )

list3 :: (MyList Int)
list3 = Empty

//toString
/*
instance toString (MyList Int)
where
	(toString) MyList = [Elem a] ++ toString*/

/*
toString :: (MyList a) -> String
toString Empty = "[]"
toString (Elem val rest) = "[" ++ (toString val) ++ concatElements rest ++ "]"
    where
        concatElements Empty = ""
        concatElements (Elem v Empty) = "," ++ [v]
        concatElements (Elem v r) = "," ++ [v] ++ concatElements r

instance toString (MyList a) 
where
    toString list = toString list*/

//Start = toString list1 // "[4,3,2,1]"
//Start = toString list2 // "[2,6,6,8,4,3,2,1]"
//Start = toString list3 // "[]"


// 9.----------------------------
/* Firsts and Middles. (10 points)
*
* Given a tree of type (TypeName String), return all the
* Node values of type FirstName or MiddleName in a list.
*/

/*

:: TypeName a = FirstName a | MiddleName a | LastName a

:: Tree a = Node a (Tree a) (Tree a) | Leaf


treeBig :: (Tree (TypeName String))
treeBig = Node (FirstName "Tariq") (Node (LastName "Forza") Leaf Leaf) ( Node (MiddleName "Beka") (Node (LastName "Arm") Leaf Leaf) (Node (MiddleName "Mohido") Leaf Leaf ))

treeRight :: (Tree (TypeName String))
treeRight = Node (FirstName "A") Leaf (Node (LastName "B") Leaf ( Node (MiddleName "C") Leaf (Node (MiddleName "D") Leaf (Node (LastName "E") Leaf Leaf))))

treeNone :: (Tree (TypeName String))
treeNone = Leaf

*/


//firstAndMiddle :: (Tree (TypeName String)) -> [String]

//Start = firstAndMiddle treeBig // ["Tariq","Beka","Mohido"]
//Start = firstAndMiddle treeRight // ["A","C","D"]
//Start = firstAndMiddle treeNone // []


// 10.----------------------------
/* One of. (10 points)
*
* Given a list of the (OneOf String Char) type,
* sum up the list as follows:
* if the type is String, add up the length of it to the total
* if the type is Char, add 1 to the total
* Note: The task is easiest if you create an instance for function toInt
*/


:: OneOf a b = A a | B b

//findWhich :: [(OneOf String Char)] -> Int

//Start = findWhich [(A "Hello"), (B 'h'), (A "This is new")] // 17
//Start = findWhich [(A "H"), (A "e"), (A "l")] // 3
//Start = findWhich [(B 'H'), (B 'e'), (B 'l')] // 3
//Start = findWhich [] //0
//----------------------------








:: Tree a = Node a (Tree a) (Tree a) | Leaf

tr1 = Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) Leaf

insert :: Int (Tree Int) -> (Tree Int)
insert n Leaf = Node n Leaf Leaf
insert n (Node e le ri)
| n <= e = Node e (insert n le) ri
= Node e le (insert n ri)

//Start = insert 1 tr1

listToTree :: [Int] -> (Tree Int)
listToTree [] = Leaf
listToTree [x:xs] = insert x (listToTree xs)


//tart = listToTree [1,10,2,20,3,4,5]


anyone knows how to do this?

:: Library = {lib_name :: String , books :: {Book}}
:: Book= {title::String,author::String, pyear :: Int,num_of_pages::Int, can_be_borrowed::Bool}

b1::Book
b1 = {title = "C Programming Language", author = "Abel" , pyear =2022 , num_of_pages = 1501 , can_be_borrowed = False }
b2::Book
b2 = {title = "Functional Programming", author = "Andrey" , pyear =1999 , num_of_pages = 1250 , can_be_borrowed = True }
b3::Book
b3 = {title = "Java Programming Language", author = "John" , pyear =1508 , num_of_pages = 2980 , can_be_borrowed =True}
b4::Book
b4 = {title = "OOP Programming", author = "Peter" , pyear =2020 , num_of_pages = 280 , can_be_borrowed = False }
b5::Book
b5 = {title = "Programming", author = "James" , pyear =2000 , num_of_pages =1645 , can_be_borrowed =True}

lib1::Library
lib1 = {lib_name = "lib1" , books ={b1,b2}}
lib2::Library
lib2 = {lib_name = "lib2" , books ={b1,b2,b3}}
lib3::Library
lib3 = {lib_name = "lib3" , books ={b1,b2,b3,b4}}
lib4::Library
lib4 = {lib_name = "lib4" , books ={b1,b4,b5}}
lib5::Library
lib5 = {lib_name = "lib5" , books ={b1,b2,b3}}
lib6::Library
lib6 = {lib_name = "lib6" , books ={b4,b4,b2,b1}}


/* 3.3 Write an instance of '+' for libraries, which unifies libraries' books,
* eliminate redundancies and arrange them according to the publication year.
* The name of new library is the concatenation of the 2 libraries' name.
*/



//Start = lib1 + lib1
//(Library "lib1lib1" {(Book "Functional Programming" "Andrey" 1999 1250 True),(Book "C Programming Language" "Abel" 2022 1501 False)})
//Start = lib1 + lib2
//(Library "lib1lib2" {(Book "Java Programming Language" "John" 1508 2980 True),(Book "Functional Programming" "Andrey" 1999 1250 True),(Book "C Programming Language" "Abel" 2022 1501 False)})