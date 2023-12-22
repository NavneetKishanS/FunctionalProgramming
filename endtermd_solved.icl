module endtermd_solved

import StdEnv


/*---------------*/
/* 1.
Given a string with no spaces, make the letters 
that occupy even indexes upper case and odd 
indexes lower case. Index 0 will be considered even.
Eg. alternate "abcdef" = "AbCdEf"
alternate "ABCdef" = "AbCdEf"
*/

alternate :: String -> String
alternate str = alternate_aux str 0

alternate_aux :: String Int -> String
alternate_aux str i
| i >= size str = ""
| isEven i = toString (toUpper str.[i]) +++ (alternate_aux str (i+1))
= toString (toLower str.[i]) +++ (alternate_aux str (i+1))

Start = alternate "abcdef" // "AbCdEf"
//Start = alternate "aaaaaaaaa" // "AaAaAaAaA"
//Start = alternate "ABCDE" // "AbCdE"
//Start = alternate "AbcDefGH" // "AbCdEfGh"
//Start = alternate "FunctionaL" // "FuNcTiOnAl"

/*---------------*/
/* 2.
Given five positive integers, find the minimum and maximum 
values  that can be calculated by summing exactly four of 
the five integers. Return the respective minimum and maximum 
values as an array of 2 elements.
Eg.	{1,3,5,7,9} -> minimum sum = 1 + 3 + 5 + 7
				   maximum sum = 3 + 5 + 7 + 9
Minimum sum is 16 and maximum sum is 24, the function returns {16,24}
*/

minMaxSum :: {Int} -> {Int}
minMaxSum arr = { sum list - maxList list , sum list - minList list }
where
	list = [a \\ a<-:arr]

//Start = minMaxSum {1,3,5,7,9} // {16,24}
//Start = minMaxSum {1,2,3,4,5} // {10,14}
//Start = minMaxSum {7,69,2,221,8974} // {299,9271}

/*---------------*/
/* 3.
Given a list of strings remove every 2nd character 
from each string and return the result list.
Eg.: ["abcdefgh", "cdcdcdc", "bababa"]
result:  ["aceg", "cccc", "bbb"]
*/

remove2ndChar :: [String] -> [String]
remove2ndChar list = [ { c \\ c <-: str & ind <- [1..] | isOdd ind} \\ str <- list ]

//Start = remove2ndChar ["abcdefgh", "cdcdcdc", "bababa"] // ["aceg","cccc","bbb"]
//Start = remove2ndChar ["faucnccdttiwoxnzaol", "pdrtosgdroazmdmbiknig"] // ["functional","programming"]
//Start = remove2ndChar ["",""] // ["", ""]
//Start = remove2ndChar [] // []

/*---------------*/
/* 4.
You are given a string s, which contains stars *.
Remove the closest non-star character to its left and right, 
as well as remove the star itself.
Return the string after all those characters are removed.
Consider other characters beside stars are not repeated in string.
*/

isAdjToStar :: String Char -> Bool 
isAdjToStar str c 
|ind==0 = (str.[(ind+1)])=='*' 
|ind==((size str)-1) = (str.[(ind-1)])=='*' 
= (str.[(ind-1)])=='*'  || (str.[(ind+1)])=='*'  
where ind = hd[y \\ x <-: str & y <- [0..] | x==c] 

rmvList :: String -> [Char]
rmvList str = [x \\ x <-: str | (isAdjToStar str x)]

rmv :: String -> String
rmv str = {y \\ y <-: str | (not (isMember y ls))&&(y <> '*')}
where ls = rmvList str

//Start = rmv "Ang9*5*6*7el8**9"//"Angel"
//Start = rmv "CD***FL2*0EANk*******i Iu*o**9S COOL*******"//"CLEAN IS COOL"
//Start = rmv "uwyesfgdbhlckjn" // uwyesfgdbhlckjn
//Start = rmv ""//""
//Start = rmv "     "//"     "
//Start = rmv "*****"//""

/*---------------*/
/* 5. 
Create an algebraic type JobLevel which can have 
three values: Junior, Intermediate, Senior.
Create a record Employee which has three fields:
employeeId - A string ID.
level - A job level with type JobLevel.
performanceScores - A list of performance scores, 
where each score is an integer.

Write a function  that takes a list of employees and 
returns the string ID of the medior employee who has 
the highest average performance score.
Assume the list contains at least one medior employee. 
If multiple medior employees have the same maximum 
average, return any of their IDs.
*/

:: JobLevel = Junior | Medior | Senior
:: Employee = {
			id :: String,
			level :: JobLevel,
			performanceScores :: [Int]
			}

employee1 = {id = "emp-1", level = Junior, performanceScores = [7, 8, 6, 8]}
employee2 = {id = "emp-2", level = Medior, performanceScores = [9, 8, 9, 10]}
employee3 = {id = "emp-3", level = Medior, performanceScores = [10, 10, 3, 9]}
employee4 = {id = "emp-4", level = Senior, performanceScores = [8, 8, 8, 8]}
employee5 = {id = "emp-5", level = Senior, performanceScores = [10, 10, 9, 5, 8]}
employee6 = {id = "emp-6", level = Junior, performanceScores = [5, 6, 5, 6]}
employee7 = {id = "emp-7", level = Medior, performanceScores = [8, 7, 8, 9]}
employee8 = {id = "emp-8", level = Senior, performanceScores = [9, 9, 10, 8]}
employee9 = {id = "emp-9", level = Medior, performanceScores = [6, 7, 5, 7]}
employee10 = {id = "emp-10", level = Senior, performanceScores = [10, 9, 8, 7, 9]}

isMedior :: Employee -> Bool
isMedior {id=_, level=Medior, performanceScores=_} = True
isMedior s = False

getBestMediorEmployee :: [Employee] -> String
getBestMediorEmployee lst = hd [a.id \\ a<-lst | avg([toReal(b) \\ b<-a.performanceScores])==maxMediorAvg]
where 
	maxMediorAvg = last(sort([avg([toReal(b) \\ b<-a.performanceScores]) \\ a<-lst | isMedior a]))
	
//Start = getBestMediorEmployee [employee1,employee2,employee3,employee4,employee5] // emp-2
//Start = getBestMediorEmployee [employee7,employee8,employee9] // emp-7
//Start = getBestMediorEmployee [employee3,employee5,employee8,employee9,employee10] // emp-3

/*---------------*/
/* 6.
Write a function reversetree that creates the mirror image of a 
binary tree, by reversing a tree where for every node left 
and right branches are interchanged.
*/
:: Tree a = Node a (Tree a) (Tree a) | Leaf

Tree1 :: Tree Int
Tree1 = Node 7 (Node 3 Leaf Leaf) (Node 9 Leaf Leaf)
Tree2 :: Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))  (Node 2 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)) 
Tree3 :: Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)

reversetree :: (Tree a) -> (Tree a)
reversetree Leaf = Leaf
reversetree (Node x le ri)=(Node x (reversetree ri) (reversetree le))

//Start = reversetree Tree1//(Node 7 (Node 9 Leaf Leaf) (Node 3 Leaf Leaf))
//Start = reversetree Tree2//(Node 0 (Node 2 (Node 6 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 1 (Node 4 Leaf Leaf) (Node 3 Leaf Leaf)))
//Start = reversetree Tree3//(Node 0 (Node 2 Leaf Leaf) (Node 1 Leaf (Node 3 (Node 8 Leaf Leaf) Leaf)))

/*---------------*/
/* 7.
Given binary tree, return the product of values of nodes 
with an odd-valued grandparent. If there are no nodes with an 
odd-valued grandparent, return -1.
A grandparent of a node is the parent of its parent if it exists.
*/
extractNode :: (Tree a) -> [a]
extractNode Leaf = []
extractNode (Node x l r) = [x]

goL :: (Tree a) -> (Tree a)
goL Leaf = Leaf
goL (Node x l r) = l

goR :: (Tree a) -> (Tree a)
goR Leaf = Leaf
goR (Node x l r) = r

getGrandChildren :: (Tree Int) -> [Int]
getGrandChildren (Node x l r) = (extractNode(goL l))++(extractNode(goR l))++(extractNode(goL r))++(extractNode(goR r))

differenceOddGrandparent :: (Tree Int) -> Int
differenceOddGrandparent Leaf = -1
differenceOddGrandparent (Node x l r)
|isOdd x = (prod(getGrandChildren (Node x l r))) + (differenceOddGrandparent l) + (differenceOddGrandparent r)
= (differenceOddGrandparent l) + (differenceOddGrandparent r)


Tree11 :: Tree Int
Tree11 = Node 15 (Node 7 (Node 9 Leaf Leaf) (Node 2 (Node 4 Leaf Leaf) Leaf))  (Node 8 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))
Tree22 :: Tree Int
Tree22 = Node 18 (Node 10 (Node 13 Leaf (Node 11 Leaf Leaf)) Leaf)  (Node 7 (Node 5 Leaf Leaf) (Node 6 Leaf (Node 12 Leaf Leaf)))
Tree33 :: Tree Int
Tree33 = Node 20 (Node 15 (Node 3 Leaf (Node 8 Leaf Leaf)) Leaf)  (Node 5 (Node 6 Leaf (Node 9 Leaf Leaf)) (Node 10 (Node 12 Leaf Leaf) Leaf))

//Start = differenceOddGrandparent Tree11 //537
//Start = differenceOddGrandparent Tree22 //6
//Start = differenceOddGrandparent Tree33 //108

/*---------------*/
/* 8.
Given a tree of any type a values, 
count all the node that has a left child (non leaf).

 		5		  => this node has a left child, so count as 1
 	  /   \
 	 3	   7      => node 3 has a left child, so count as 1, node 7 has a left child, so count as 1
 	/ \   / \
   2   4 6	 L	  => none of these has a left child
Result: 3, total 3 nodes have a left child
	
 		5		  => this node has a left child, so count as 1
 	  /   \
 	 3	   7      => node 3 has no children, count as 0, node 7 has a left child, so count as 1
 	/ \   / \
   L   L 6	 8	  => none of these has a right child 
Result: 2, total 2 nodes have a right child
   
   		5		  => this node has no left child, so count as 0
 	  /   \
 	 L	   3      => node 3 has no children, count as 0
 	      / \  	
         L   L		  
Result: 0, no node has a right child
*/

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

treeA = Node 5 (Node 3 (Node 2 Leaf Leaf) (Node 4 Leaf Leaf)) (Node 7 (Node 6 Leaf Leaf) Leaf)
treeB = Node 5 (Node 3 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) (Node 8 Leaf Leaf))
treeC = Node 5 Leaf (Node 3 Leaf Leaf) 
treeD = Node 5 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)

countNodes :: (Tree a) -> Int
countNodes Leaf = 0
countNodes (Node a left right)
| isLeaf left = countNodes left + countNodes right
= 1 + countNodes left + countNodes right

//Start = countNodes treeA // 2
//Start = countNodes treeB // 2
//Start = countNodes treeC // 0
//Start = countNodes treeD // 1

/*---------------*/
/* 9.
Overload the (-) operator for lists. It will return a list of the
elements that are a part of the first list and are not in the second.
Eg. [1,2,3,4] - [1,3,5] = [2,4]
[1,3] are common elements for both lists, so [2,4] are unique for the first list.
*/

instance - [a] | Eq a
where
	(-) [] list2 = []
	(-) [x:xs] list2
	| isMember x list2 = xs - list2
	= [x : (xs - list2)]
	
//Start = [1,2,3,4] - [1,3,5] // [2,4]
//Start = [2,4,6,7,8] - [2,4,6,7,8] // []
//Start = [12,52,23,56,23,43,32] - [42,43,32,12,57,13] // [52,23,56,23]
//Start = [12,21] - [] // [12,21]
//Start = [] - [1,2] // []

/*---------------*/
/* 10.
Construct the new type synonym called ChildIntList 
which is defined for list of integers. Write a function that 
takes a ChildIntList and an integer, insert that 
integer into ChildIntList at the front position 
and return the result ChildIntList.
If the integer already exists in the ChildIntList, 
abort it with "Already Existed" message.
*/

:: ChildIntList :== [Int]

insertChildIntList :: ChildIntList Int -> ChildIntList
insertChildIntList list x
| isMember x list = abort "Already Existed"
= [x] ++ list

//Start = insertChildIntList [6,7,8,9,0,1] 8 // "Already Existed"
//Start = insertChildIntList [6,7,9,0,1] 8 // [8,6,7,9,0,1]
//Start = insertChildIntList [6,7,8,9,0,1] 2 // [2,6,7,8,9,0,1]
//Start = insertChildIntList [] 8 // [8]

/*---------------*/
/* 11.
Write a function that takes a ChildIntList and an integer, 
and removes the integer from ChildIntList and return the result 
ChildIntList. If the integer does not exist in the ChildIntList, 
abort it with "Does not Exist" message.
*/

removeChildIntList :: ChildIntList Int -> ChildIntList
removeChildIntList list x
| isMember x list = [a \\ a<- list | a <> x] 
= abort "Does not Exist" 

//Start = removeChildIntList [6,7,8,9,0,1] 0 // [6,7,8,9,1]
//Start = removeChildIntList [6,7,9,0,1] 9 // [6,7,0,1]
//Start = removeChildIntList [6,7,8,9,0,1] 2 // "Does not Exist"
//Start = removeChildIntList [] 8 // "Does not Exist"

/*---------------*/
/* 12.
Write a function that takes a ChildIntList and an integer, 
and counts the integer from ChildIntList and return the result. 
If the integer does not exist in the ChildIntList, 
abort it with "Does not Exist" message.
*/

countChildIntList :: ChildIntList Int -> Int
countChildIntList list x
| isMember x list = sum [1 \\ a<- list | a == x] 
= abort "Does not Exist" 

//Start = countChildIntList [6,7,8,9,0,0] 0 // 2
//Start = countChildIntList [1,6,7,1,0,1] 1 // 3
//Start = countChildIntList [6,7,8,9,0,1] 2 // "Does not Exist"
//Start = countChildIntList [] 8 // "Does not Exist"
/*---------------*/