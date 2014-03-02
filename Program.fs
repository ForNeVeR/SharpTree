module Splay.Tests

open Splay.Tree

let test1() =
    let tree = insert 1 "a" empty
    let value, _ = find 1 tree
    assert (value = Some "a")    

[<EntryPoint>]
let main argv = 
    test1()
    printfn "All tests ok"
    0