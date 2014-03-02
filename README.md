SharpTree
=========

This is an implementation of the [splay
tree](http://en.wikipedia.org/wiki/Splay_tree) algorithms written in
F#.

`Sharp.Tree` API is deadly simple: there are `empty` instance and
`find`, `insert` and `remove` functions for searching and modifying
the tree. Note that the `find` function also returns a modified
version of the tree itself (this is a special feature of the splay
tree: search operation may modify the tree).