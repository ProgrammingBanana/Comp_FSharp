


let rec power base_num pow =
    if pow = 0
        then 1
        else base_num * power base_num (pow-1)

let rec power' base_num pow =
    match pow with
        | 0 -> 1
        | _ -> base_num * power base_num (pow-1)

let rec power'' base_num = function
    | 0 -> 1
    | pow -> base_num * power base_num (pow-1)

let power''' base_num pow =
    let rec helper base_num accum = function
        | 0 -> accum
        | pow -> helper base_num (accum*base_num) (pow-1)

    helper base_num 1 pow

let powerOf2 = Seq.initInfinite(power''' 2)


let num_base = 2
let pow = 8

printfn "Calculating the power of base %d and power %d" num_base pow
printfn "Using a tail-recursive helper function, the term is %d" (power''' num_base pow)
printfn "Using a pattern matching function, the term is %d" (power'' num_base pow)
printfn "Using a pattern matching expression, the term is %d" (power' num_base pow)
printfn "Using an if expression, the term is %d" (power num_base pow)
printfn "The infinite sequence  power of two are %A" powerOf2

