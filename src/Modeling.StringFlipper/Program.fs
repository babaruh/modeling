open System
open Xunit
open FsUnit.Xunit

let reverseString (str: string) =
    str
    |> Seq.rev
    |> String.Concat

printfn "Please enter a string:"
let userInput = Console.ReadLine()

let result = reverseString userInput

printfn "Output:"
printfn $"%s{result}"


[<Fact>]
let ``reverseString "" should be ""`` () =
    reverseString "" |> should equal ""

[<Fact>]
let ``reverseString "hello" should be "olleh"`` () =
    reverseString "hello" |> should equal "olleh"
    
[<Fact>]
let ``reverseString "tiger" should be "regit"`` () =
    reverseString "tiger" |> should equal "regit"

[<Fact>]
let ``reverseString "racecar" should be "racecar"`` () =
    reverseString "racecar" |> should equal "racecar"

[<Fact>]
let ``reverseString "F# is fun" should be "nuf si #F"`` () =
    reverseString "F# is fun" |> should equal "nuf si #F"
