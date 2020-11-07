

let minEven lst =
    if List.isEmpty lst
        then failwith "FAIL: The list is empty"
        else if List.filter(fun elem -> elem % 2 = 0) lst |> List.isEmpty
            then failwith "FAIL: The list has no even numbers"
            else List.filter(fun elem -> elem % 2 = 0) lst |> List.min


let lst = [30; 10; 50; 40; 20]
let no_evens = [31; 11; 51; 41; 21]
let empty = []

printfn "The minimum even number is %d" (minEven lst)
printfn "The minimum even number is %d" (minEven no_evens)
printfn "The minimum even number is %d" (minEven empty)

