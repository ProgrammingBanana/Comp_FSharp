

let rec sumOfSquares = function
    |[] -> 0
    |hd::tl -> hd*hd + sumOfSquares  tl

let sumOfSquares' lst =
    let rec helper accum = function
        |[] -> accum
        |hd::tl -> helper (accum + hd*hd) tl

    helper 0 lst

let sumOfSquares'' lst = List.map(fun elem -> elem * elem) lst 
                        |> List.fold(fun elem acc -> acc + elem) 0


let lst = [1;2;3;4]


printfn "The sum of the squares in the list using List.map and piping to List.fold is %d" (sumOfSquares'' lst)
printfn "The sum of the squares in the list using a tail-recursive helper function is %d" (sumOfSquares' lst)
printfn "The sum of the squares in the list using regular recusion is %d" (sumOfSquares lst)