module Project01_07

//
// reduce F L
//
// reduces L to a single value by applying function F
// 
// Examples: reduce (fun x y -> x+y) []             => Unhandled Exception: System.ArgumentException: The input list was empty.
//           reduce (fun x y -> x*y) [23; 4]        => 92
//           reduce (fun x y -> x+y) [23; 43; -60]  => 6 
//           reduce (fun x y -> if x > y then x else y) 
//                  ['c'; 'a'; 'n'; 'a'; 'd'; 'a']  => 'n' 
// 
// You may not call List.reduce directly in your solution.
// 
// 

let reduce F L =
    if L = [] then
        raise (System.ArgumentException "The input list was empty.")

    let rec _reduce F L x = 
        match L with
        | [] -> x
        | elem::[] -> F x elem
        | elem::tail -> 
            _reduce F tail (F x elem)
    _reduce F (List.tail L) (List.head L)


//[<EntryPoint>]
let main argv =
    printfn "Testing Project 07: reduce"

    let red1 = reduce (fun x y -> x&&y) [false]
    if red1 = false then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let red2 = reduce (fun x y -> x*y) [23; 4]
    if red2 = 92 then
        printfn "Passed!"
    else
        printfn "Failed!"
        
    let red3 = reduce (fun x y -> x+y) [23; 43; -60]
    if red3 = 6 then
        printfn "Passed!"
    else
        printfn "Failed!"

    let input4 = ['c'; 'a'; 'n'; 'a'; 'd'; 'a']
    let red4 = reduce (fun x y -> if x > y then x else y) input4
    if red4 = 'n' then
        printfn "Passed!"
    else
        printfn "Failed!"

    printfn ""
    0 // return an integer exit code
    
