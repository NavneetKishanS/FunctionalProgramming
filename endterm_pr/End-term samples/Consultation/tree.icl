module tree
import StdEnv

// recursion 
// first call the function again to initiate recursion
// stop the recursion when you need

// Examples
:: Tree a = Node a (Tree a) (Tree a) 
          | Leaf

trS :: Tree String
trS = Node "Apples" (Node "Pear" (Node "B" Leaf Leaf) (Node "C" Leaf Leaf)) (Node "ABC" Leaf Leaf)

//      Apple
//   Pear    ABC
// B     C

// ApplePearABC , PEARBC ,ABC , B , C
// Tree1 see link: http://graphonline.ru/en/?graph=RDODcKkbEjpzIbIh
Tree1 :: Tree Int
Tree1 = Node 7 Leaf Leaf

// Tree2 see link: http://graphonline.ru/en/?graph=apYgfCbqYeaQRHNL
Tree2 :: Tree Int
Tree2 = Node 0 (Node 1 (Node 3 Leaf (Node 5 Leaf Leaf)) (Node 5 Leaf Leaf))  (Node 11 (Node 7 Leaf Leaf) (Node 9 Leaf Leaf)) 
//Start = Tree2

// Tree3 see link: http://graphonline.ru/en/?graph=YMMkGtZycajcoXEU
Tree3 :: Tree Int
Tree3 = Node 0 (Node 1 (Node 3 Leaf (Node 11 Leaf Leaf)) Leaf)  (Node 2 Leaf Leaf)

//3,6,9,12

//1. Count Nodes which are multiples of 3
count3Nodes :: (Tree Int) -> Int
count3Nodes Leaf = 0
count3Nodes (Node x le ri) 
//| x == 0 = (count3Nodes le) + (count3Nodes ri)
| x rem 3 == 0 = 1 + (count3Nodes le) + (count3Nodes ri)
= (count3Nodes le) + (count3Nodes ri)


//Start = count3Nodes Tree2 // 2

//[1,2,3] ++ [] = [1,2,3]

//2. Put all the even nodes in a list
evenNodeList :: (Tree Int) -> [Int]
evenNodeList Leaf = []
evenNodeList (Node x le ri)
|isEven x = [x] ++ (evenNodeList le) ++ (evenNodeList ri)
= (evenNodeList le) ++ (evenNodeList ri)


//3. put all nodes in a list
nList :: (Tree a) -> [a]
nList Leaf = []
nList (Node x le ri) =  (nList le) ++ [x] ++ (nList ri)

//Start = nList trS


/*
func :: (Tree a) -> b
func Leaf = something of type b that doesnt change the output(0,1,[],"",Leaf)
func (Node a le ri) = func le (+,*,++,+++) func ri*/



//Start = evenNodeList Tree3

// True or Flase or False = t
// True and False and false = f

//3. Check if atleast one node has value more than 10  // or ||
check10 :: (Tree Int) -> Bool
check10 Leaf = False
check10 (Node x le ri)
| x>10 = True || check10 le || check10 ri
= False || check10 le || check10 ri
	
//Start = check10 Tree3


//4. check if all nodes has negative values // &&
nNodes :: (Tree Int) -> Bool
nNodes Leaf = True
nNodes (Node x le ri) = (x<0) && (nNodes le) && (nNodes ri)

/*| x<0 = True && (nNodes le) && (nNodes ri)
= False && (nNodes le) && (nNodes ri)*/


//Start = nNodes (Node -1 (Node 2 Leaf Leaf) Leaf)

extractNode :: (Tree a) -> a
extractNode (Node x le ri) = x

//5. check if for all nodes the value at node is equal to the sum of the value at nodes of left and right child
// and

sumChild :: (Tree Int) -> Bool
sumChild Leaf = True
sumChild (Node x Leaf Leaf) = True
sumChild (Node x Leaf ri) = (x == extractNode ri) && sumChild ri
sumChild (Node x le Leaf) = (x == extractNode le) && sumChild le
sumChild (Node x le ri) = (x == extractNode le + extractNode ri) && sumChild le && sumChild ri



tr1 :: Tree Int
tr1 = Node 5 (Node 3 (Node 2 Leaf Leaf) (Node 1 Leaf Leaf)) (Node 2 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf))

//     5
//  3    2
// 2  1 1  1

//Start = sumChild tr1


extractNodeString :: (Tree String) -> String
extractNodeString Leaf = ""
extractNodeString (Node x le ri) = x
//6. JOIN THE PARENT STRING WITH THE CHILDREN STRINGS

joinPC :: (Tree String) -> [String]
joinPC Leaf = []
joinPC (Node x le ri) = [x +++ (extractNodeString le) +++ (extractNodeString ri)] ++ (joinPC le) ++ (joinPC ri)

//Start = joinPC trS




//8. replace the nodes that are more than 5 with 0
repNode :: (Tree Int) -> (Tree Int)
repNode Leaf = Leaf
repNode (Node x le ri)
| x > 5 = Node 0 (repNode le) (repNode ri)
= Node x (repNode le) (repNode ri)

//Start = repNode Tree2

lenS :: String -> Int
lenS s = length y
	where y = [a \\ a<-:s]

//Start = lenS {'A','r','y','a','n'} // "Aryan" === {'A','r','y','a','n'}

/*
func :: (Tree a) -> b
func Leaf = something of type b that doesnt change the output(0,1,[],"",Leaf)
func (Node a le ri) = func le (+,*,++,+++) func ri*/
//steps
// handle the stopping case (when you have leaf)
// call the function for the le and ri child
// join the outputs together

isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False




//7.  JOIN THE PARENT STRING WITH THE CHILDREN STRINGS left + parent + right

joinStr :: (Tree String) -> [String]
joinStr Leaf = []
joinStr (Node x le ri)
| lenS x <= 3 = [(extractNodeString le) +++ x +++ (extractNodeString ri)] ++ joinStr le ++ joinStr ri
= joinStr le ++ joinStr ri

//Start = joinStr trS

// for all nodes check if both children have odd values

oddChildren :: (Tree Int) -> Bool 
oddChildren Leaf = True
oddChildren (Node x le ri)
|isOdd (extractNodeInt le) && isOdd (extractNodeInt ri) = True && oddChildren le && oddChildren ri
= False


extractNodeInt :: (Tree Int) -> Int
extractNodeInt Leaf = 1
extractNodeInt (Node x le ri) = x

//Start = oddChildren Tree2

// check if there is atleast one node where  both children have odd values

atLeast :: (Tree Int) -> Bool
atLeast Leaf = False
atLeast (Node x le ri)
|isOdd (extractNodeInt le) && isOdd (extractNodeInt ri) = True || atLeast le || atLeast ri
= False || atLeast le || atLeast ri

//Start = atLeast Tree2

// take a string tree and then put all nodes in a list

inOrder :: (Tree a) -> [a]
inOrder Leaf = []
inOrder (Node x le ri) = (inOrder le) ++ [x] ++ (inOrder ri)

//Start = putNodes trS

// take a string tree and put the nodes in a list // only those nodes whose length is more than 5

putStrList :: (Tree String) -> [String]
putStrList tree = [e \\ e <- inOrder tree | lenS e > 5]

//Start = putStrList trS

// collect those nodes in a list whose left child is a Leaf
collectNodes :: (Tree a) -> [a]
collectNodes Leaf = []
collectNodes (Node x Leaf Leaf) = []
collectNodes (Node x Leaf ri) = [x] ++ collectNodes ri
collectNodes (Node x le Leaf) = collectNodes le 
collectNodes (Node x le ri) = collectNodes le ++ collectNodes ri

Start = collectNodes Tree2



