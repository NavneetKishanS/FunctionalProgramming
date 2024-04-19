module PT5gr13
import StdEnv


/* 
WRITE NAME AND NEPTUN: 

   Given the list of tuples, where each tuple
 * contains name and the points of the student.
 * Write a function that will anotate each data
 * with "Fail" or "Pass" tag. Each tuple should
 * be changed with a new tuple, which in addition
 * to name and grade will contain this tag. Tag
 * should be "Pass" if student has more than 40 
 * points, otherwise - "Fail".
 */


passOrFail :: Int -> String
passOrFail x
| x >40 = "Pass"
= "Fail"

task :: [(String, Int)] -> [(String, Int, String)]
task [] = []
task ls = [ (a,b,passOrFail b) \\ a<-(fst (unzip (ls))) & b<-(snd (unzip (ls))) ]

//Start = task [("A",91),("B", 36),("C",78)] // [("A",91,"Pass"),("B",36,"Fail"),("C",78,"Pass")]
//Start = task [("A",91),("B", 35),("C",78),("D",12),("E",34)] // [("A",91,"Pass"),("B",35,"Fail"),("C",78,"Pass"),("D",12,"Fail"),("E",34,"Fail")] 
//Start = task [] // []
//Start = task [("A",100),("B", 85),("C",78)] // [("A",100,"Pass"),("B",85,"Pass"),("C",78,"Pass")]
//Start = task [("A",12)] // [("A",12,"Fail")]
