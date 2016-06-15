module MultiwayTree exposing
    ( Tree (..), Forest
    , datum, children
    , map
    )

{-| A library for constructing multi-way trees. Each Tree carries two pieces of
information, it's datum and children.


# Types
@docs Tree, Forest

# Operations
@docs datum, children

# Mapping
@docs map
-}

{-| A type to keep track of datum and children.
-}
type Tree a = Tree a (Forest a)


{-| A list of Trees. Convenient for describing children.
-}
type alias Forest a = List (Tree a)


{-| Access the datum of the current tree
-}
datum : Tree a -> a
datum (Tree datum children) = datum


{-| Access the children of the current tree
-}
children : Tree a -> Forest a
children (Tree datum children) = children


{-| Map over the MultiwayTree
-}
map : (a -> b) -> Tree a -> Tree b
map fn (Tree datum children) =
    let mappedDatum = fn datum
        mappedChildren = List.map (\child -> map fn child) children
    in
        (Tree mappedDatum mappedChildren)
