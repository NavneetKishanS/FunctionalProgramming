module PT910gr13
import StdEnv

// Write your name and neptun code 

:: Major = CS | Math | Physics | Linguistics
:: Course = {cname::String, major :: Major, credits:: Int, max_student:: Int}

Programming::Course
Programming = {cname="Programming",major=CS, credits =5, max_student=50}

Functional::Course
Functional = {cname="Functional",major=CS,credits=5, max_student=120}

Imperative::Course
Imperative = {cname="Imperative",major=CS,credits=4, max_student=150}

Analysis::Course
Analysis = {cname="Analysis",major=Math, credits =4, max_student=85}

BasicMath::Course
BasicMath = {cname="BasicMath",major=Math,credits=2, max_student=40}

Hungarian::Course
Hungarian = {cname="Hungarian",major=Linguistics,credits=3, max_student=20}


/* Create an instance `+` for the record Course, 
   such that for two records C1 and C2 when added (C1+C2) will give
   a record Course where:
 			- name is the concatenation of the first letter in C1's and C2's names.
 			- major is the course with the greatest max_student.
 			- credits is the greatest credit if courses have same major,
 				or the average of credits, if the courses have different major 
 				(you don't have to use real number for average)
 			- max_student is C1's max_student plus C2's max_student.
*/

nameModifier :: Course Course -> String
nameModifier a b = {a.cname.[0]} +++ {b.cname.[0]}

getMajor :: Course Course -> Major
getMajor a b 
| a.max_student > b.max_student = a.major
= b.major

courseList = [Programming, Functional, Imperative, Analysis, BasicMath, Hungarian]

instance == Major
where
	(==) CS CS = True
	(==) Math Math = True
	(==) Linguistics Linguistics = True
	(==) _ _ = False

highestCredit::[Course] Major -> Int
highestCredit ls m = last (sort[x.credits\\x <- ls | x.major == m])
//where ls = courseList

//Start = highestCredit courseList CS
/*
getCredit :: Course Course -> Int
getCredit a b 
| a.major == b.major = */

//Start = getMajor Functional Imperative

//Start = nameModifier Functional Imperative

/*
instance + Course
where 
	(+) C1 C2 = nameModifier C1 C2 getMajor C1 C2 highestCredit [C1,C2]  (C1.max_student + C2.max_student)*/

//Start = Functional

//Start = Functional + Imperative // (Course "FI" CS 5 270)
//Start = BasicMath + Hungarian // (Course "BH" Math 2 60)
//Start = Analysis + Programming // (Course "AP" Math 4 135)
//Start = Imperative + BasicMath // (Course "IB" CS 3 190)



//:: PriorityQueue ...

//remove :: (PriorityQueue a) (Int,a) -> (PriorityQueue a)


//Start = remove [(1,2), (2,582), (3,52114), (15, 8996)] (3,52114) // [(1,2),(2,582),(15,8996)]
//Start = remove [] (5,50) // []
//Start = remove [(5,True), (23,True), (56,False), (89, False)] (5,True)//[(23,True),(56,False),(89,False)]

