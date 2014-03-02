namespace Sharp.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Splay.Tree

[<TestClass>]
type SharpTreeTest() =
    [<TestMethod>]
    member this.mapTest() =
        let map = Map.ofList <| [for x in [1..10] -> x, x.ToString()]
        let tree = ref empty<int, string>
        Map.iter (fun k v -> tree := insert k v !tree) map
        Map.iter (fun k v -> let v', tree' = find k !tree
                             tree := tree'
                             Assert.AreEqual(Some v, v')) map