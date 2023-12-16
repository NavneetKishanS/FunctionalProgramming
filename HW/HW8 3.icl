module HW8
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

:: School = HighSchool | College | University

:: Role = Teacher | Student

:: Subject = Math | Science | History | Geography | Literature

/*
Create a record Person with the following fields:
id::Int
school::School
scores::[Int]
subjects::[Subject]
role::Role
*/

:: Person = { id::Int
   ,school::School
   ,scores::[Int]
   ,subjects::[Subject]
   ,role::Role
    }
/*
You are given an array of citizens , write a function that returns the list of (id,uni,rel) pair of citizens 
who will be awarded. a Citizen can be awarded if he/she has at least 3 courses , the average of his/her grades is at least 3.

*/
GPA :: [Int] -> Int
GPA arr = (sum[a \\ a<- arr])/(length arr)

//Start = GPA [80,70,85,60,90]


EligibleForScholarship :: {Person} -> [(Int,School,Role)]
//EligibleForScholarship p = []
EligibleForScholarship personArr = [(person.id,person.school,person.role) \\ person <-: personArr | (GPA person.scores) >= 75 && (length (person.scores)) >= 3 ]


//Start =  EligibleForScholarship {{id = 1, school = HighSchool, scores = [80,70,85,60,90], subjects = [Math,Science,History], role = Student}, {id = 2, school = College, scores = [80,85,90,75,95], subjects = [Math,Literature], role = Student}, {id = 3, school = University, scores = [80,70,90,95,100], subjects = [Math], role = Teacher}}
//[(1,HighSchool,Student)]

//Start =  EligibleForScholarship {{id = 1, school = HighSchool, scores = [80,70,85,60,90], subjects = [Math,Science,History,Geography], role = Student}, {id = 2, school = College, scores = [80,85,90,75,95], subjects = [Math,Science,History,Literature], role = Student}, {id = 3, school = University, scores = [80,70,90,95,100], subjects = [Math,Science,History], role = Teacher}}
//[(1,HighSchool,Student), (2,College,Student), (3,University,Teacher)]

//Start =  EligibleForScholarship {} // []

//EXTRA TEST CASE: Person1: subjects < 3	,	Person2: GPA average < 75
//Start =  EligibleForScholarship {{id = 1, school = HighSchool, scores = [80,70], subjects = [Math,Science], role = Student}, {id = 2, school = College, scores = [8,58,30,75,95], subjects = [Math,Science,History,Literature], role = Student}, {id = 3, school = University, scores = [80,70,90,95,100], subjects = [Math,Science,History], role = Teacher}}


/*
Write a function that takes an array of Strings and removes consonants from each string.
*/


RemoveConsonants :: {String} -> {String}
RemoveConsonants wordsArr = {remConst word \\ word<-:wordsArr}

//to keep only the vowels
remConst :: String -> String
remConst word = {vowel\\vowel<-[ch \\ ch<-:word |(isMember ch ['a','e','i','o','u','A','E','I','O','U'])]}

//Start = remConst "Clean"

//Start = RemoveConsonants {"apple","banana","cherry","date"} // {"ae","aaa","e","ae"}
//Start = RemoveConsonants {"Eloquent","JavaScript","Code"} // {"Eoue","aai","oe"}
//Start = RemoveConsonants {"Functional","Programming","is","fun"} // {"uioa","oai","i","u"}
