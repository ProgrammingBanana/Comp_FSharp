(*
 * File: power.fs
 * Author: Daniel De Jesus 802-16-1676
 * Course COTI 4039 – Comparative Programming Languages
 * Date: Noviembre 7, 2020
 * Program that computes the power of a given base and exponent
 *)

// power: int int -> int
// Computes the power of a given base and exponent using
// regular recursion and if statement
let rec power base_num pow =
    if pow < 0
        then failwith "Exponent needs to be 0 or greater"
        else if pow = 0
            then 1
            else base_num * power base_num (pow-1)

// power': int int -> int
// Computes the power of a given base and exponent using 
// a pattern matching expression
let rec power' base_num pow =
    match pow with
        | 0 -> 1
        | _ -> if pow < 0 then 
                failwith "Exponent needs to be 0 or greater" 
                else base_num * power' base_num (pow-1)

// power'': int int -> int
// Computes the power of a given base and exponent using
// a pattern matching function
let rec power'' base_num = function
    | 0 -> 1
    | pow -> if pow < 0 then 
                failwith "Exponent needs to be 0 or greater" 
                else base_num * power'' base_num (pow-1)

// power''': int int -> int
// Computes the power of a given base and exponent using
// a tail-recursive helper function
let power''' base_num pow =
    // power''': int int int -> int
    // Computes the power of a given base and exponent using
    // a tail-recursion
    let rec helper base_num accum = function
        | 0 -> accum
        | pow -> helper base_num (accum*base_num) (pow-1)

    if pow < 0
        then failwith "Exponent needs to be 0 or greater"
        else helper base_num 1 pow

// powerOf2: seq<int>
// Generates an infite sequence for the all the powers of 2
let powerOf2 = Seq.initInfinite(power''' 2)

// Entry point for the program
let num_base = 2
let pow = 8

// Tests the variations of the power functions
printfn "Calculating the power of base %d and power %d" num_base pow
printfn "Using a tail-recursive helper function, the term is %d" (power''' num_base pow)
printfn "Using a pattern matching function, the term is %d" (power'' num_base pow)
printfn "Using a pattern matching expression, the term is %d" (power' num_base pow)
printfn "Using an if expression, the term is %d" (power num_base pow)
printfn "The infinite sequence  power of two are %A" powerOf2

