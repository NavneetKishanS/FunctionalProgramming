module ex_tree2_todo

import StdEnv


// 1. Given a tree and an integer. Find all the nodes that are equal to the 
// integer and give the sum of their direct children. (Leaf count as 0).
//exNode :: (Tree Int) -> Int

preorder :: (Tree a) -> [a] 
preorder Leaf = []
preorder (Node x le ri) = [x] ++ preorder le ++  preorder ri


f10 :: (Tree Int) Int -> Int
f10 Leaf a = 0
f10 (Node x le re) a
| (extractNode(Node x le re) == a) = (extractNode le)+ (extractNode re) + (f10 le a) + (f10 re a)
= (f10 le a) + (f10 re a)

/*
extractLR :: (Tree Int) -> Int
extractLR Leaf = -1
extractLR (Node x le ri) = le + ri
*/

extractNode :: (Tree Int) -> Int
extractNode Leaf = -1
extractNode (Node x le ri) = x

//Start = extractNode(Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf))

//Start = f10 (Node 2 Leaf Leaf) 3 // 0
//Start = f10 (Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) 3 // 2
//Start = f10 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf)) 1 // 2
//Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))) 2 // 7
//Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 Leaf (Node 1 Leaf Leaf))) 2 // 4



// 2. Given a tree and an integer n, find the nodes equal to n and 
// replace by 0.

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

replace :: Int (Tree Int) -> (Tree Int) 
replace y Leaf = Leaf
replace y (Node x le re) 
| x == y = Node 0 (replace y le) (replace y re)
= Node x (replace y le) (replace y re)

atree = Node 4 (Node 3 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf)(Node 7 Leaf Leaf))

//Start = replace 3 atree  
//(Node 4 (Node 0 (Node 1 Leaf Leaf) (Node 0 Leaf Leaf)) (Node 6 (Node 0 Leaf Leaf) (Node 7 Leaf Leaf)))



// 3. Add "_over18" to the name of persons that are over age of 18 in a tree of persons. 

:: Person = { name::String
			, birthday::(Int,Int,Int)
	        }

t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2005,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (1999,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2003,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2005,11,23)} Leaf Leaf)

//Start = t1
//Start = t2
//Start = t3


over18 :: (Int,Int,Int) -> Bool
over18 (a,b,c) 
| a >= 2004 = False
= True

addString :: Person -> Person
addString a = {a & name = a.name +++ "_over18"}

//Start = ((extractNode t2).name) +++ "_over18"
//Start = addString {name = "hh", birthday = (2001,11,22)}

updateName :: (Tree Person) -> (Tree Person)
updateName Leaf = Leaf
updateName (Node x le ri)
| (over18 x.birthday) = Node (addString x) (updateName le) (updateName ri)
= Node x (updateName le) (updateName ri)

//Start = updateName t2 
//(Node (Person "hh" (2005,11,22)) 
//(Node (Person "hr_over18" (2001,11,21)) Leaf Leaf) 
//(Node (Person "ht_over18" (2001,11,23)) Leaf Leaf))

//Start = updateName t3 
//(Node (Person "hh_over18" (1999,11,22)) 
//(Node (Person "hr_over18" (2001,11,21)) 
//(Node (Person "hh_over18" (2003,11,22)) Leaf Leaf) 
//(Node (Person "hh_over18" (1998,11,22)) Leaf Leaf)) 
//(Node (Person "ht" (2005,11,23)) Leaf Leaf))



// 4. You are given a binary tree.
// Check if it is a binary search tree (BST).
// In BST values in left subtree should be 
// less then the current node's value and 
// values in right subtree should be greater.

:: BST a = BSTNode a (BST a) (BST a) | BSTLeaf


isBST :: (BST Int) -> Bool
isBST t = x == sort x
where x = treeToList t
/*
isBST Leaf = False
isBST BSTNode a 
| */ 

treeToList :: (BST a) -> [a]
treeToList BSTLeaf = []
treeToList (BSTNode x le ri) = treeToList le ++ [x] ++ treeToList ri

// For testing.
bst1 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 3 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 12 (BSTNode 5 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst2 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 9 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))
bst3 = (BSTNode 1 BSTLeaf (BSTNode 20 (BSTNode 3 (BSTNode 9 BSTLeaf BSTLeaf) (BSTNode 4 BSTLeaf (BSTNode 1 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf))) (BSTNode 45 (BSTNode 34 (BSTNode 22 BSTLeaf BSTLeaf) BSTLeaf) (BSTNode 112 (BSTNode 53 BSTLeaf BSTLeaf) BSTLeaf))))
bst4 = (BSTNode 1 BSTLeaf (BSTNode 2 (BSTNode 7 BSTLeaf (BSTNode 12 (BSTNode 12 (BSTNode 8 BSTLeaf BSTLeaf) BSTLeaf) BSTLeaf)) BSTLeaf))

Start = map isBST [bst1,bst2,bst3,bst4,BSTLeaf] // [True,True,False,False,True]




