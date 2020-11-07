(*
 * File: ins_sort_quiz.fs
 * Author: Daniel De Jesus 802-16-1676
 * Course COTI 4039 – Comparative Programming Languages
 * Date: Noviembre 7, 2020
 * Program that inserts into a sorted list and sorts unsorted lists 
 *)


// insert: 'a 'a list -> 'a list
// Inserts a number into an ordered list in its correct position
// using a tail-recursive helper function
let insert num lst =
    // helper: 'b 'b list 'b list -> 'b list
    // Inserts a number into an ordered list in its correct position
    // using a tail recursion
    let rec helper num lst2 = function
        |[] -> lst2@[num]
        |hd::tl -> if num < hd 
                    then lst2@num::hd::tl 
                    else helper num (lst2@[hd]) tl
                            

    helper num [] lst
    

// insertion_sort: 'a list -> 'a list
// Sorts a list using a tail-recusive helper function
let insertion_sort lst =
    // helper: 'b list 'b list -> 'b list
    // Sorts a list by using tail recusion and applying the insert function
    let rec helper lst2 = function
        |[] -> lst2
        |hd::tl -> helper (insert hd lst2) tl

    helper [] lst


// Entry point for the program
// Tests the insert and insertion_sort functions
printfn "Empty list inserting with tail recursion %A" (insert 4 [])
printfn "Sorted list inserting with tail recursion %A" (insert 4 [1;2;3;5])
printfn "Insertion sort using tail recursion %A" (insertion_sort [5;10;4;3;2;9;1;6;-7;8])
printfn "Insertion sort on empty list %A" (insertion_sort [])