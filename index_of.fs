(*
 * File: index_of.fs
 * Author: Daniel De Jesus 802-16-1676
 * Course COTI 4039 – Comparative Programming Languages
 * Date: Noviembre 7, 2020
 * Finds the index of an element in the list and if its not
 * in the list it returns -1
 *)

// indexOf: 'a 'a list -> int
// Finds the position of the element using a tail-recursive 
// helper function if not found returns -1
let indexOf num lst = 
    // indexOf: int int 'b 'b list -> int
    // Finds the position of the element using a tail recursion 
    // if not found returns -1
    let rec helper index position num = function
        |[]-> position
        |hd::tl -> if num = hd then 
                        helper (index+1) index num tl 
                        else helper (index+1) position num tl
    
    helper 0 (-1) num lst

// Entry point for the program
let lst = [30; 40; 10; 50; 20]
let lst_empty = []
let number = 10
let number_not_in_list = 11

// Tests the index_of function when the number is in the list, the number
// is missing or the list is empty
printfn "The position of the number %d in the empty list is %d" number (indexOf number lst_empty)
printfn "The position of the number %d (not in the list) is %d" number_not_in_list (indexOf number_not_in_list lst)
printfn "The position of the number %d in the list is %d" number (indexOf number lst)