module MultiwayTree
    ( Tree (..), Forest
    , datum, children
    )
    where

{-| A library for constructing multi-way trees. Each Tree carries two pieces of
information, it's datum and children.
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
