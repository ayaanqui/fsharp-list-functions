module Project01_04

//
// nth L n
//
// Returns nth element of L
// 
// Examples: nth []   0       => raises an exception
//           nth [94] 2      => raises an exception
//           nth [94]  0      => 94
//           nth [94]  -1      => raises an exception 
//           nth [1; 45; 6] 1 => 45
//           nth [1; 45; 6] 5 => raises an exception
//           nth ['q'; 'w'; 'e'; 'r'; 't'; 'y'] 5 => 'y'
// You may not call List.nth, List.Item, .[], etc directly in your solution.
// 
let nth L n =
    if n >= (List.length L) or n < 0 then
        raise (System.IndexOutOfRangeException "Index out of bounds")

    let rec _nth L n pos cur = 
        if n = pos then 
            cur
        else 
            _nth (List.tail L) n (pos + 1) (List.head L)

    _nth (List.tail L) n 0 (List.head L)


//[<EntryPoint>]
let main argv =
    printfn "Testing Project 04: nth"

    let nth2 = nth [94] 0
    if nth2 = 94 then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let nth3 = nth [1; 45; 6] 1
    if nth3 = 45 then
        printfn "Passed!"
    else
        printfn "Failed!"

    let nth7 = nth ['q'; 'w'; 'e'; 'r'; 't'; 'y'] 5
    if nth7 = 'y' then
        printfn "Passed!"
    else
        printfn "Failed!"
      
    printfn ""
    0 // return an integer exit code
    

