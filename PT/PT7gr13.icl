module PT7gr13
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf
         
/*  WRITE NAME AND NEPTUN !!! HERE: NAVNEET KISHAN SRINIVASAN - JZD5GY
	Given a tree with positive values up to 999, check if each node is less than both its 
	left child and its right child. A leaf is bigger then any node value.
					 10
				   /     \
				  18      17
				 / \     / \
				24  27  26  25
			   / \ / \ / \ / \
			  32 L L L L L L  44
			  
	That tree given returns TRUE because each node is less than 
	both its left child and its right child.

*/

treeOne = Node 10 (Node 18 (Node 24 (Node 32 Leaf Leaf) Leaf) (Node 27 Leaf Leaf)) (Node 17 (Node 26 Leaf Leaf) (Node 25 Leaf (Node 44 Leaf Leaf)))
treeTwo = Node 10 (Node 2 (Node 4 (Node 9 Leaf Leaf) Leaf) (Node 7 Leaf Leaf)) (Node 3 (Node 6 Leaf Leaf) (Node 5 Leaf (Node 1 Leaf Leaf))) 
treeThree = Node 10 (Node 20 (Node 40 (Node 80 Leaf Leaf) Leaf) (Node 700 Leaf Leaf)) (Node 30 (Node 60 Leaf Leaf) (Node 100 Leaf (Node 200 Leaf Leaf))) 
 
smaller :: (Tree Int) -> Bool 
smaller Leaf = True
smaller (Node root ltree rtree) 
|(root < getNode ltree) && (root < getNode rtree) && (smaller ltree) && (smaller rtree) = True
= False

getNode :: (Tree Int) -> Int
getNode Leaf = 999
getNode (Node root ltree rtree) = root

//Start = smaller treeOne		// True
//Start = smaller treeTwo		// False
//Start = smaller (Node 10 Leaf Leaf)  // True
//Start = smaller treeThree	// True
//Start = smaller Leaf // True