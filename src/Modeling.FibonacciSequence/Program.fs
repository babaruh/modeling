open Xunit
open FsUnit.Xunit

let fib n =
    let rec loop acc1 acc2 = function
        | n when n = 0I -> acc1
        | n when n = 1I -> acc2
        | n -> loop acc2 (acc1 + acc2) (n - 1I)
    loop 0I 1I n
    
printfn $"fibonacci sequence of 10:\t%O{fib 10I}" 
printfn $"fibonacci sequence of 1:\t%O{fib 1I}" 
printfn $"fibonacci sequence of 200:\t%O{fib 200I}" 


[<Fact>]
let ``fib 0 should be 0`` () =
    fib 0I |> should equal 0I

[<Fact>]
let ``fib 1 should be 1`` () =
    fib 1I |> should equal 1I

[<Fact>]
let ``fib 10 should be 55`` () =
    fib 10I |> should equal 55I
