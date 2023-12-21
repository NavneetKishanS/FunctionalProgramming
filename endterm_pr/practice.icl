module practice
import StdEnv

//Start = [1..5]

instance + String
where 
	(+)s1 s2 = s1 +++ s2

//Start = "Hello" + "World"	

instance + Bool
where
	(+) True True = True
	(+) True False = False
	(+) False True = False
	(+) False False = True
	(+) _ _ = False
	
//Start = False + True

:: Major = Finance | CS | Math | Physics | Economy | Linguistics
Major1=Finance
Major2=CS

instance == Major 
where
	(==) Finance Finance = True
	(==) CS CS = True
	(==) Math Math = True
	(==) Physics Physics = True
	(==) Economy Economy = True
	(==) Linguistics Linguistics = True
	(==) _ _ = False
	
//Start = Major1 == Finance

instance + [Int]
where 
	(+) l1 l2 = l1 ++ l2
	
//Start = [1..3] + [1..4]

::Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 = Node 10 (Node 5 (Node 3 (Node 1 Leaf Leaf) (Node 9 Leaf Leaf)) (Node 7 Leaf Leaf)) (Node 15 (Node 12 Leaf Leaf) (Node 18 Leaf Leaf))
tree2 = Node 12 (Node 8 (Node 4 Leaf Leaf) (Node 7 (Node 6 Leaf Leaf) Leaf)) (Node 10 Leaf (Node 15 Leaf (Node 14 Leaf Leaf)))
transformTree :: (Tree Int) -> (Tree Int)
transformTree tree = transformTreeHelper tree 1

transformTreeHelper :: (Tree Int) Int -> (Tree Int)
transformTreeHelper Leaf _ = Leaf
transformTreeHelper (Node val left right) depth =
    Node (val * depth) (transformTreeHelper right (depth + 1)) (transformTreeHelper left (depth + 1))

//Start = transformTree tree1 

isAnagram :: String String -> Bool   
isAnagram str1 str2 = sum[1 \\ a <-: str1 , b <-: str2 | a == b ] == size str1
//Start = isAnagram "listen" "silent" // True