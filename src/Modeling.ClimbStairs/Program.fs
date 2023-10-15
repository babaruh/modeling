open System
open Xunit
open FsUnit.Xunit

let rec climbStairs n =
    match n with
    | _ when n < 0 -> invalidArg "n" "n must be non-negative"
    | 0 -> 0 
    | 1 -> 1 
    | 2 -> 2
    | _ -> climbStairs (n - 1) + climbStairs (n - 2)

printfn "Amount of Steps:"
let userInput = Console.ReadLine() |> int

let result = climbStairs userInput

printfn "Output:"
printfn $"%i{result}"

[<Fact>]
let ``climbStairs -1 should throw an exception when n is negative`` () =
    Assert.Throws<ArgumentException> (fun () -> climbStairs -1 |> ignore)
    
[<Fact>]
let ``climbStairs 0 should be 0`` () =
    climbStairs 0 |> should equal 0

[<Fact>]
let ``climbStairs 1 should be 1`` () =
    climbStairs 1 |> should equal 1
    
[<Fact>]
let ``climbStairs 3 should be 3`` () =
    climbStairs 3 |> should equal 3

[<Fact>]
let ``climbStairs 10 should be 89`` () =
    climbStairs 10 |> should equal 89
