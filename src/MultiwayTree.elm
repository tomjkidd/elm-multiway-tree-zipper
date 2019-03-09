module MultiwayTree exposing
    ( Tree(..), Forest
    , datum, children, foldl, foldr, flatten, tuplesOfDatumAndFlatChildren, filter, filterWithChildPrecedence, length, insertChild, appendChild
    , map, mapListOverTree, indexedMap
    , sortBy, sortWith
    )

{-| A library for constructing multi-way trees. Each Tree carries two pieces of
information, it's datum and children.


# Types

@docs Tree, Forest


# Operations

@docs datum, children, foldl, foldr, flatten, tuplesOfDatumAndFlatChildren, filter, filterWithChildPrecedence, length, insertChild, appendChild


# Mapping

@docs map, mapListOverTree, indexedMap


# Sorting

@docs sortBy, sortWith

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
datum (Tree d c) =
    d


{-| Access the children of the current tree
-}
children : Tree a -> Forest a
children (Tree d c) =
    c


{-| Inserts a Tree as the first child of a Tree
-}
insertChild : Tree a -> Tree a -> Tree a
insertChild childTree (Tree d c) =
    Tree d (childTree :: c)


{-| Inserts a Tree as the last child of a Tree
-}
appendChild : Tree a -> Tree a -> Tree a
appendChild childTree (Tree d c) =
    Tree d (c ++ [ childTree ])


{-| Reduce a Tree from the left.
-}
foldl : (a -> b -> b) -> b -> Tree a -> b
foldl f accu (Tree d c) =
    let
        treeUnwrap (Tree datum_ children_) accu_ =
            List.foldl treeUnwrap (f datum_ accu_) children_
    in
    List.foldl treeUnwrap (f d accu) c


{-| Reduce a Tree from the right.
-}
foldr : (a -> b -> b) -> b -> Tree a -> b
foldr f accu (Tree d c) =
    let
        treeUnwrap (Tree datum_ children_) accu_ =
            f datum_ (List.foldr treeUnwrap accu_ children_)
    in
    f d (List.foldr treeUnwrap accu c)


{-| Flattens a Tree into a List where the root is the first element of that list.
-}
flatten : Tree a -> List a
flatten tree =
    foldr (::) [] tree


{-| A special version of flatten which flattens a Tree into a List of Tuples like (element, [all elements in subtree])

    Tree.tuplesOfDatumAndFlatChildren
        Tree
        "a"
        [ Tree "b" []
        , Tree "c" []
        , Tree "d" []
        ]
        == [ ( "a", [ "b", "c", "d" ] ), ( "b", [] ), ( "c", [] ), ( "d", [] ) ]

-}
tuplesOfDatumAndFlatChildren : Tree a -> List ( a, List a )
tuplesOfDatumAndFlatChildren (Tree d c) =
    [ ( d, List.concatMap flatten c ) ] ++ List.concatMap tuplesOfDatumAndFlatChildren c


{-| Return the length of the Tree. Calculated recusively as datum (1) + length of children (n)
Since a MultiwayTree is never empty this function will never return Int < 1.
-}
length : Tree a -> Int
length tree =
    foldr (\_ accu -> accu + 1) 0 tree


{-| Map over the MultiwayTree
-}
map : (a -> b) -> Tree a -> Tree b
map fn (Tree d c) =
    let
        mappedDatum =
            fn d

        mappedChildren =
            List.map (\child -> map fn child) c
    in
    Tree mappedDatum mappedChildren


{-| Map a Function over a List and a MultiwayTree.
-}
mapListOverTree : (a -> b -> result) -> List a -> Tree b -> Maybe (Tree result)
mapListOverTree fn list (Tree d c) =
    case list of
        [] ->
            Nothing

        head :: [] ->
            let
                mappedDatum =
                    fn head d
            in
            Just (Tree mappedDatum [])

        head :: rest ->
            let
                mappedDatum =
                    fn head d

                listGroupedByLengthOfChildren =
                    splitByLength (List.map length c) rest

                mappedChildren =
                    List.map2 (\l child -> mapListOverTree fn l child) listGroupedByLengthOfChildren c
                        |> List.filterMap identity
            in
            Just (Tree mappedDatum mappedChildren)


splitByLength : List Int -> List a -> List (List a)
splitByLength listOflengths list =
    splitByLength_ listOflengths list []


splitByLength_ : List Int -> List a -> List (List a) -> List (List a)
splitByLength_ listOflengths list accu =
    case listOflengths of
        [] ->
            List.reverse accu

        currentLength :: restLengths ->
            case list of
                [] ->
                    List.reverse accu

                head :: rest ->
                    splitByLength_ restLengths (List.drop currentLength list) (List.take currentLength list :: accu)


{-| Same as map but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> Tree a -> Maybe (Tree b)
indexedMap f tree =
    mapListOverTree f (List.range 0 (length tree - 1)) tree


{-| Filter the MultiwayTree. Keep only elements whose datum satisfy the predicate.
-}
filter : (a -> Bool) -> Tree a -> Maybe (Tree a)
filter predicate (Tree d c) =
    if predicate d then
        Just (Tree d (List.filterMap (filter predicate) c))

    else
        Nothing


{-| Filter the MultiwayTree. If the predicate is True for a Child the entire path to the root will be part of the result Tree.
-}
filterWithChildPrecedence : (a -> Bool) -> Tree a -> Maybe (Tree a)
filterWithChildPrecedence predicate (Tree d c) =
    case List.filterMap (filterWithChildPrecedence predicate) c of
        [] ->
            if predicate d then
                Just (Tree d [])

            else
                Nothing

        children_ ->
            Just (Tree d children_)


{-| Sort values by a derived property. Does not alter the nesting structure of
the Tree, that is it does not move Nodes up or down levels.

    sortBy identity
        Tree
        "a"
        [ Tree "b" []
        , Tree "d" []
        , Tree "c" []
        ]
        == Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

-}
sortBy : (a -> comparable) -> Tree a -> Tree a
sortBy fn (Tree d c) =
    let
        sortedChildren =
            List.sortBy (\(Tree childDatum children_) -> fn childDatum) c
                |> List.map (sortBy fn)
    in
    Tree d sortedChildren


{-| Sort values with a custom comparison function like:

    flippedComparison a b =
        case compare a b of
          LT -> GT
          EQ -> EQ
          GT -> LT

    This is also the most general sort function, allowing you
    to define any other.

-}
sortWith : (a -> a -> Order) -> Tree a -> Tree a
sortWith comperator (Tree d c) =
    let
        sortedChildren =
            List.sortWith (\(Tree first _) (Tree second _) -> comperator first second) c
                |> List.map (sortWith comperator)
    in
    Tree d sortedChildren
