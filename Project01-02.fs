module Project01_02

//
// max L
//
// Returns maximum element of L
// 
// Examples: max []          => raises an exception (Unhandled Exception: System.ArgumentException: The input sequence was empty.)
//           max [-2; 4]     => 4
//           max [34]        => 34
//           max [10; 10; 9] => 10
//           max ['a'; 'e'; 'c'] => e
// 
// You may not call List.max directly in your solution.
// 


let max L =
    let greater n1 n2 = if (n1 > n2) then n1 else n2

    let rec _max L hi = 
        match L with
        | [] -> hi
        | elem::tail -> _max tail (greater hi elem)

    _max (List.tail L) (List.head L)


//[<EntryPoint>]
let main argv =
    printfn "Testing Project 02: max"

    let max2 = max [-2; 4]
    if max2 = 4 then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let max3 = max [34]
    if max3 = 34 then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let max4 = max [10; 10; 9]
    if max4 = 10 then
        printfn "Passed!"
    else
        printfn "Failed!" 
        
    let max5 = max ['a'; 'e'; 'c']
    if max5 = 'e' then
        printfn "Passed!"
    else
        printfn "Failed!"
    
    printfn ""
    0 // return an integer exit code
    

