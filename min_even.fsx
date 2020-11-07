(*
 * File: min_even.fs
 * Author: Daniel De Jesus 802-16-1676
 * Course COTI 4039 – Comparative Programming Languages
 * Date: Noviembre 7, 2020
 * Finds the value of the smallest even number if none are present
 * it fails out of the program.
 *)

// indexOf: int list -> int
// Finds the smallest even number by using List.filter and piping List.min. 
// If no even numbers are found or the list is empty it fails out of the program
let minEven lst =
    if List.isEmpty lst
        then failwith "FAIL: The list is empty"
        else if List.filter(fun elem -> elem % 2 = 0) lst |> List.isEmpty
            then failwith "FAIL: The list has no even numbers"
            else List.filter(fun elem -> elem % 2 = 0) lst |> List.min

// Entry point of the program
let lst = [30; 10; 50; 40; 20]
let no_evens = [31; 11; 51; 41; 21]
let empty = []

// Testing minEven function
printfn "The minimum even number is %d" (minEven lst)

// Delete comments to test failwith in function
// printfn "The minimum even number is %d" (minEven no_evens)
// printfn "The minimum even number is %d" (minEven empty)

