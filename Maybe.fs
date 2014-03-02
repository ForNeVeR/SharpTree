module Maybe

type MaybeBuilder() =
    member x.Bind(v,f) = Option.bind f v
    member x.Return v = Some v
    member x.ReturnFrom o = o

let maybe = MaybeBuilder()