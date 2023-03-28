open System
// Solution

// In this sample, the goal was to compute the length of the list

// Method 1: Library Function
let max_second (list: 'a list) =
  if (list.Length = 1) then failwith "List has only one element"
  else
    let list = list |> List.filter (fun x -> x < List.max list)
    if list.IsEmpty then failwith "No second max element"
    List.max list


           
(* // Method 2: Recursion
let rec max_second2 list  =
  let list = sort list *)
let rec max_second_simple_rec lst first_max second_max =
    match lst with
    | [] -> failwith "List is empty"
    | [_] -> failwith "List has only one element"
    | [a; b] -> 
      if a = b then (a, Int32.MinValue)
      else (max a b, min a b)
    | h::t -> 
        let first_max, second_max = max_second_simple_rec t first_max second_max
        if h > first_max then
          let second_max = first_max
          let first_max = h
          (first_max, second_max)  
        elif h < first_max && h > second_max && h <> second_max then
          let second_max = h
          (first_max, second_max)  
        else (first_max, second_max)
      
      


// Method 3: Tail Rec
let max_second_tail_rec list =
    let max1 = Int32.MinValue
    let max2 = Int32.MinValue
    let rec find_second_max'_tail max1 max2 list= 
      match list with
      | [] -> failwith "Empty list"
      | [x] -> if x > max1 && max1 <> Int32.MinValue then max1 
                    elif max2 = Int32.MinValue then failwith "No second max elem"
                    else max2

      | h::t -> if(h > max1) then
                  let max2 = max1
                  let max1 = h
                  find_second_max'_tail max1 max2 t
                else 
                  find_second_max'_tail max1 max2 t
    find_second_max'_tail max1 max2 list

let main = 
  printfn "%A" (max_second [2;2;3])
  let first_max_rec, second_max_rec = max_second_simple_rec [2;2] Int32.MinValue Int32.MinValue
  if second_max_rec = Int32.MinValue then failwith "No second max elem"
  else printfn "%A" second_max_rec
  printfn "%A" (max_second_tail_rec [2;2;3])


main