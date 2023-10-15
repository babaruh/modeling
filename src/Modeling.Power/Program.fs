open System
open Xunit
open FsUnit.Xunit

let rec pow (x: float) (n: float) =
    match n with
    | 0.0 -> 1.0
    | 1.0 -> x
    | -1.0 -> 1.0 / x
    | _ when n % 2.0 = 0.0 -> pow (x * x) (n / 2.0)
    | _ -> x * pow (x * x) ((n - 1.0) / 2.0)

let splitAndConvert (s: string) (sep: char) =
  let parts = s.Split sep |> List.ofArray
  if parts.Length >= 2 then
    let x = parts.[0] |> float
    let n = parts.[1] |> float
    (x, n)
  else
    (0.0, 0.0)

printfn "Enter x and n"
let userInput = Console.ReadLine()
let x, n = splitAndConvert userInput ' '
let test = pow x n
printfn $"power of %f{x} to %f{n} is %f{test}"


[<Fact>]
let ``pow(5,2) should be 25``() =
    pow 5.0 2.0 |> should equal 25.0

[<Fact>]
let ``pow(2,-2) should be 0.25``() =
    pow 2.0 -2.0 |> should equal 0.25

[<Fact>]
let ``pow(3,3) should be 27``() =
    pow 3.0 3.0 |> should equal 27.0
