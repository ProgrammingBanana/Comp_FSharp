

let indexOf num lst = 
    let rec helper index position num = function
        |[]-> position
        |hd::tl -> if num = hd then 
                        helper (index+1) index num tl 
                        else helper (index+1) position num tl

    helper 0 (-1) num lst

let lst = [30; 40; 10; 50; 20]
let lst_empty = []
let number = 10
let number_not_in_list = 11

printfn "The position of the number %d in the empty list is %d" number (indexOf number lst_empty)
printfn "The position of the number %d (not in the list) is %d" number_not_in_list (indexOf number_not_in_list lst)
printfn "The position of the number %d in the list is %d" number (indexOf number lst)