(*
 * File: square_sum.fs
 * Author: Daniel De Jesus 802-16-1676
 * Course COTI 4039 – Comparative Programming Languages
 * Date: Noviembre 7, 2020
 * Program that computes the squares of a list
 *)

// sumOfSquares: int list -> int
// Finds the sum of the squares of the elements of the list using regular recursion
let rec sumOfSquares = function
    |[] -> 0
    |hd::tl -> hd*hd + sumOfSquares  tl

// sumOfSquares: int list -> int
// Finds the sum of the squares of the elements of the list using a tail-recursive 
// helper function
let sumOfSquares' lst =
    // sumOfSquares: int int list -> int
    // Finds the sum of the squares of the elements of the list using tail recursion
    let rec helper accum = function
        |[] -> accum
        |hd::tl -> helper (accum + hd*hd) tl

    helper 0 lst

// sumOfSquares: int list -> int
// Finds the sum of the squares of the elements of the list using List.map 
// and List.fold and lambda functions 
let sumOfSquares'' lst = List.map(fun elem -> elem * elem) lst 
                        |> List.fold(fun elem acc -> acc + elem) 0

// Entry point of the program                    
let lst = [1;2;3;4]

// Testing the variations of the sumOfSquares function and different condition
printfn "The sum of the squares in the list using List.map and piping to List.fold is %d" (sumOfSquares'' lst)
printfn "The sum of the squares of empty list using List.map and piping to List.fold is %d" (sumOfSquares'' [])
printfn "The sum of the squares in the list using a tail-recursive helper function is %d" (sumOfSquares' lst)
printfn "The sum of the squares of empty list using a tail-recursive helper function is %d" (sumOfSquares' [])
printfn "The sum of the squares in the list using regular recusion is %d" (sumOfSquares lst)
printfn "The sum of the squares of empty list using regular recusion is %d" (sumOfSquares [])