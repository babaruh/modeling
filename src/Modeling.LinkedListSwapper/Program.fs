open Xunit
open FsUnit.Xunit

type Node<'T> = 
    | Empty
    | Cons of 'T * Node<'T>

let rec swapPairs (head: Node<'T>) : Node<'T> =
    match head with
    | Empty -> Empty 
    | Cons (x, Empty) -> Cons (x, Empty)
    | Cons (x, Cons (y, tail)) -> Cons (y, Cons (x, swapPairs tail)) 

let list1 = Cons (5, Cons (6, Cons (7, Empty)))
let list2 = swapPairs list1
printfn $"%A{list2}"
let list3 : Node<int> = swapPairs Empty
printfn $"%A{list3}"
let list4 = swapPairs (Cons (1, Empty))
printfn $"%A{list4}"

[<Fact>]
let ``swapPairs Empty should be Empty`` () =
    swapPairs Empty |> should equal Empty

[<Fact>]
let ``swapPairs Cons(1, Empty) should be Cons(1, Empty)`` () =
    swapPairs (Cons(1, Empty)) |> should equal (Cons(1, Empty))

[<Fact>]
let ``swapPairs Cons(1, Cons(2, Empty)) should be Cons(2, Cons(1, Empty))`` () =
    swapPairs (Cons(1, Cons(2, Empty))) |> should equal (Cons(2, Cons(1, Empty)))

[<Fact>]
let ``swapPairs Cons(1, Cons(2, Cons(3, Cons(4, Empty)))) should be Cons(2, Cons(1, Cons(4, Cons(3, Empty))))`` () =
    swapPairs (Cons(1, Cons(2, Cons(3, Cons(4, Empty))))) |> should equal (Cons(2, Cons(1, Cons(4, Cons(3, Empty)))))
