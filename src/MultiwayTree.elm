module MultiwayTree
    exposing
        ( Tree(..)
        , Forest
        , datum
        , children
        , map
        , filter
        , filterWithChildPrecedence
        , flatten
        , foldr
        , foldl
        )

{-| A library for constructing multi-way trees. Each Tree carries two pieces of
information, it's datum and children.


# Types
@docs Tree, Forest

# Operations
@docs datum, children, foldl, foldr, flatten, filter, filterWithChildPrecedence

# Mapping
@docs map
-}


{-| A type to keep track of datum and children.
-}
type Tree a
    = Tree a (Forest a)


{-| A list of Trees. Convenient for describing children.
-}
type alias Forest a =
    List (Tree a)


{-| Access the datum of the current tree
-}
datum : Tree a -> a
datum (Tree datum children) =
    datum


{-| Access the children of the current tree
-}
children : Tree a -> Forest a
children (Tree datum children) =
    children


{-| Reduce a Tree from the left.
-}
foldl : (a -> b -> b) -> b -> Tree a -> b
foldl f accu (Tree datum children) =
    let
        treeUnwrap (Tree datum' children') accu' =
            List.foldl treeUnwrap (f datum' accu') children'
    in
        List.foldl treeUnwrap (f datum accu) children


{-| Reduce a Tree from the right.
-}
foldr : (a -> b -> b) -> b -> Tree a -> b
foldr f accu (Tree datum children) =
    let
        treeUnwrap (Tree datum' children') accu' =
            f datum' (List.foldr treeUnwrap accu' children')
    in
        f datum (List.foldr treeUnwrap accu children)


{-| Flattens a Tree into a List where the root is the first element of that list.
-}
flatten : Tree a -> List a
flatten tree =
    foldr (::) [] tree


{-| Map over the MultiwayTree
-}
map : (a -> b) -> Tree a -> Tree b
map fn (Tree datum children) =
    let
        mappedDatum =
            fn datum

        mappedChildren =
            List.map (\child -> map fn child) children
    in
        (Tree mappedDatum mappedChildren)


{-| Filter the MultiwayTree. Keep only elements whose datum satisfy the predicate.
-}
filter : (a -> Bool) -> Tree a -> Maybe (Tree a)
filter predicate (Tree datum children) =
    if predicate datum then
        Just (Tree datum (List.filterMap (filter predicate) children))
    else
        Nothing


{-| Filter the MultiwayTree. If the predicate is True for a Child the entire path to the root will be part of the result Tree.
-}
filterWithChildPrecedence : (a -> Bool) -> Tree a -> Maybe (Tree a)
filterWithChildPrecedence predicate (Tree datum children) =
    case List.filterMap (filterWithChildPrecedence predicate) children of
        [] ->
            if predicate datum then
                Just (Tree datum [])
            else
                Nothing

        children' ->
            Just (Tree datum children')
