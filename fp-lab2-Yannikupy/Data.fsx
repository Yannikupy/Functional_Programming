// Part 2

// Load the data
#load "Four.fsx"
open Four

// Task 1.1
let calculateAverageGradesByGroup (students : (int * string * (string * int) list) list) =
    students
    |> List.groupBy (fun (group, _, _) -> group)
    |> List.map (fun (group, students) ->
        let totalGrade =   students|> List.collect (fun (_, _, grades) -> grades)|> List.map snd |> List.sum
        let subjsNumber = List.length Four.subjs
        let studentsCount = students |> List.map(fun (_,_,grades) -> grades) |> List.length 
        (group, float totalGrade / (float studentsCount *  float subjsNumber)))


let findFailingStudentsForSubject studs subjs =
    subjs |> List.iter (fun subjCode ->
    let failingStudents = studs |> List.filter (fun (_, _, subjList) ->
        subjList |> List.exists (fun (subj, grade) -> subj = fst subjCode && grade = 2)
    )
    let subjName = subjs |> List.find (fun (code, name) -> code = fst subjCode) |> snd
    printfn "Студенты, провалившие экзамен по %s:" subjName
    failingStudents |> List.iter (fun (_, surname, _) -> printfn "- %s" surname))

let filterBadGrades (group, surname, grades) =
    let twoCount = grades |> List.filter (fun (_, g) -> g = 2) |> List.length
    match twoCount with
    | 0 | 1 -> (group, surname, grades)
    | _ ->
        let filteredGrades =
            grades
            |> List.filter (fun (_, g) -> g <> 2)
            |> fun gs -> gs @ [("AnySubject", 2)]
        (group, surname, filteredGrades)

let countFailedStudents (studs: (int * string * (string * int) list) list) (subjs: (string * string) list) =
    let filtered_studs = studs |> List.map filterBadGrades 
    filtered_studs |> List.groupBy (fun (group, _, _) -> group) 
    |> List.map (fun (group, students) ->
        let failed = students
                     |> List.collect (fun (student, _, grades) -> grades)
                     |> List.filter (fun (subj, grade) -> grade = 2) |> List.length 
                    
        (group, failed))

printfn "Средние оценки :"
for (group, avg_mark) in calculateAverageGradesByGroup Four.studs do
  printfn "%d: %f" group avg_mark
findFailingStudentsForSubject Four.studs Four.subjs
printfn "Число несдавших студентов в каждой из групп"
for (group, failed) in countFailedStudents Four.studs Four.subjs do
  printfn "%d: %d" group failed