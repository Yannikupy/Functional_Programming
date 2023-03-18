let eps = 0.001
let dichotomy f a b =
    let rec dic a b =
        let c = (a + b) / 2.0
        if ((f c) = 0.) || ((c - a) < eps) then c
        elif f a * f c < 0.0 then dic a c
        else dic c b
    dic a b
    

let iterations phi x0 =
    let rec iterate x =
        let x' = phi x
        if abs(x - x') < eps then x'
        else iterate x'
    iterate x0

let newthon f f' x0 =
    let phi x = x - f x / f' x
    iterations phi x0

let f1 x = x * x - log(1. + x) - 3.
let f2 x = 2. * x * sin(x) - cos(x)
let f3 x = exp(x) + sqrt(1. + exp(2. * x)) - 2.

let f1' x = 2. * x - 1. / (1. + x)
let f2' x = 3. * sin(x) + 2. * x * cos(x)
let f3' x = exp(x) + (exp(2.*x) / sqrt(1. + exp(2. * x)))

let phi1 x = sqrt(log(1. + x) + 3.)
let phi2 x = (cos x) / (2. * sin x)
let phi3 x = log(2. - sqrt(1. + exp(2. * x)))

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 2. 3.) (iterations phi1 2.) (newthon f1 f1' 2.)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 0.4 1.) (iterations phi2 0.4) (newthon f2 f2' 0.4)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 -1. 0.) (iterations phi3 -1.) (newthon f3 f3' -1.)