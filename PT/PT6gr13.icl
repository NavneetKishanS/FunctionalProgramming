module PT6gr13

import StdEnv

/* Create a structure `Student` with 3 fields: name (string), id (string)
 * grades (array of integers).
 * Write a function getGradebook that takes array of students and returns array
 * of tuples, where first element is the id of the student, second one is average
 * of it's grades and the 3rd one is a string "Passed" if the average is greater or 
 * equal to 65.5, "Failed" otherwise. Average should NOT be rounded
 * to integers, it should be real.
 */

:: Student ={name::String, id::String,grades :: {Int}}

getIndividualGrade :: Student -> (String, Real, String)
getIndividualGrade s = (s.name,(getGradeAvg s.grades),passOrFail(getGradeAvg s.grades ))

getGradebook :: {Student} -> {(String, Real, String)}
getGradebook sArr = [e]
//{s:sx} = {(s.name,(getGradeAvg s.grades),passOrFail(getGradeAvg s.grades ))} ++ getGradebook {sx}

getGradeAvg :: {Int} -> Real
getGradeAvg arr = (sum[toReal(e) \\ e<-: arr]) / toReal(length[toReal(e) \\ e<-: arr])

passOrFail :: Real -> String
passOrFail x
| x >= 65.5 = "Passed"
= "Failed"

//Start = getGradeAvg {80,40,70}
//Start = getIndividualGrade student4
//Start = getGradebook {student1}

// Intended for tests. Do not remove!
student1 = {name="a",id="st1",grades={80,40,70}}
student2 = {name="b",id="st2",grades={120,30,80,40,70}}
student3 = {name="c",id="st3",grades={80,50,40,70}}
student4 = {name="d",id="st4",grades={}}