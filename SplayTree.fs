module Splay.Tree

open Maybe

type Node<'k, 'v> = {
    left  : SplayTree<'k, 'v>
    right : SplayTree<'k, 'v>
    key   : 'k
    value : 'v
} and SplayTree<'k, 'v> = Node<'k, 'v> option

let empty<'k, 'v> : SplayTree<'k, 'v> = None

let splay topGetter bottomGetter topSetter bottomSetter tree =
    match tree with
    | None -> None
    | Some node ->
        let parent = node
        let top = Option.get <| topGetter node
        let bottom = bottomGetter node
        let top' = topGetter top
        let bottom' = parent |> topSetter (bottomGetter top)
                             |> bottomSetter bottom
        let parent' = top |> topSetter top'
                          |> bottomSetter (Some bottom')

        Some parent'

// Create some lenses:
let private left = fun node -> node.left
let private right = fun node -> node.right
let private setLeft = fun value node -> { node with left = value }
let private setRight = fun value node -> { node with right = value }

let splayLeft tree = splay left right setLeft setRight tree
let splayRight tree = splay right left setRight setLeft tree

let zigZig splay1 splay2 = splay1 >> splay2

let zigZigLeft tree = zigZig splayLeft splayLeft tree
let zigZigRight tree = zigZig splayRight splayRight tree

let zigZag parentSplay grandParentSplay topGetter topSetter tree =
    let grandParent = Option.get tree
    let parent = topGetter grandParent
    let parent' = parentSplay parent
    let grandParent' = topSetter parent' grandParent
    grandParentSplay <| Some grandParent'

let zigZagLeft tree = zigZag splayRight splayLeft left setLeft tree
let zigZagRight tree = zigZag splayLeft splayRight right setRight tree

let rec find (key : 'k)
             (tree : SplayTree<'k, 'v>)
             : 'v option * SplayTree<'k, 'v> =
    match tree with
    | None -> None, tree
    | Some node when key = node.key -> Some node.value, tree
    | Some node when key < node.key ->
        match node.left with
        | None                          -> None, tree
        | Some left when key = left.key -> Some left.value, splayLeft tree
        | Some left when key < left.key -> find key (zigZigLeft tree)
        | Some left when key > left.key -> find key (zigZagLeft tree)
        | _ -> failwith "Impossible"
    | Some node when key > node.key ->
        match node.right with
        | None                          -> None, tree
        | Some right when key = right.key -> Some right.value, splayRight tree
        | Some right when key < right.key -> find key (zigZagRight tree)
        | Some right when key > right.key -> find key (zigZigRight tree)
        | _ -> failwith "Impossible"
    | _ -> failwith "Impossible"

let split key tree =
    let _, tree' = find key tree
    match tree' with
    | None -> None, None
    | Some node when key = node.key ->
        node.left, node.right
    | Some node when key < node.key ->
        let left = node.left
        let right = Some { node with left = None }
        left, right
    | Some node when key > node.key ->
        let left = Some { node with right = None }
        let right = node.right
        left, right
    | _ -> failwith "Impossible"

let insert key value tree =
    let left, right = split key tree
    Some { left = left
           right = right
           key = key
           value = value }

let merge left right =
    match left with
    | None   -> right
    | Some _ ->
        match right with
        | None        -> left
        | Some right' ->
            let _, parent = find right'.key left
            Some { (Option.get parent) with right = right }

let remove key tree =
    let _, parent = find key tree
    match parent with
    | None      -> None
    | Some node ->
        merge node.left node.right