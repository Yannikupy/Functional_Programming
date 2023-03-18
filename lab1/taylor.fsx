// Print a table of a given function f, computed by taylor series

// function to compute
let f x = log(1. + x - 2. * x * x)

let a = -0.2
let b = 0.3
let n = 10

// Define a function to compute f using naive taylor series method
let rec taylor_naive x n i acum eps = 
    let last_term = x**(i - 1.) * ((-1.) ** (i) * (2. ** (i - 1.)) - 1.) / (i - 1.)
    let new_term = x**i * (((-1.) ** (i + 1.) * (2. ** i) - 1.) / i)
    if(i =1. || abs(new_term - last_term) >= eps) then
        let acum = acum + new_term
        taylor_naive x n (i + 1.) acum eps
    else
        (acum, i) 

// Define a function to do the same in a more efficient way
let rec taylor x n i acum last_term eps = 
    let new_term = -((2. * ((-2.) ** i + 1.) * (i - 1.) * x) / (((-2.) ** i - 2.) * i))
    if(i = 2. || abs(last_term * new_term - last_term) >= eps) then
        let last_term = last_term * new_term
        let acum = acum + last_term
        taylor x n (i + 1.) acum last_term eps
    else 
        (acum, i)

let print tuple1 =
   match tuple1 with
    | (a, b) -> printfn "Pair %A %A" a b

let main =
   printfn "%s\t%s\t\t%s\t%s\t%s" "x" "Builtin" "Smart Taylor" "# terms" "Dumb Taylor '# terms" 
   let eps = 0.001
   for i=1 to n do
     let x = a+(float i)/(float n)*(b-a)
     let value_taylor_naive, terms_taylor_naive = taylor_naive x i 1. 0 eps
     let value_taylor, terms_taylor = taylor x i 2. x x eps
     printfn "%5.2f   %10.6f  %10.6f \t\t%0.0f\t %10.6f \t %0.0f" x (f x) value_taylor terms_taylor value_taylor_naive terms_taylor_naive
// make sure to improve this table to include the required number of iterations
// for each of the methods

main