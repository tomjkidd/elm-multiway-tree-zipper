module MultiwayTree
    exposing
        ( Tree(..)
        , Forest
        , datum
        , children
        , map
        , mapListOverTree
        , indexedMap
        , filter
        , filterWithChildPrecedence
        , flatten
        , tuplesOfDatumAndFlatChildren
        , foldr
        , foldl
        , length
        , insertChild
        , appendChild
        )

{-| A library for constructing multi-way trees. Each Tree carries two pieces of
information, it's datum and children.


# Types
@docs Tree, Forest

# Operations
@docs datum, children, foldl, foldr, flatten, tuplesOfDatumAndFlatChildren, filter, filterWithChildPrecedence, length, insertChild, appendChild

# Mapping
@docs map, mapListOverTree, indexedMap
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


{-| Inserts a Tree as the first child of a Tree
-}
insertChild : Tree a -> Tree a -> Tree a
insertChild childTree (Tree datum children) =
    Tree datum (childTree :: children)


{-| Inserts a Tree as the last child of a Tree
-}
appendChild : Tree a -> Tree a -> Tree a
appendChild childTree (Tree datum children) =
    Tree datum (children ++ [ childTree ])


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


{-| A special version of flatten which flattens a Tree into a List of Tuples like (element, [all elements in subtree])

    (Tree.tuplesOfDatumAndFlatChildren
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ])
    == [ ( "a", [ "b", "c", "d" ] ), ( "b", [] ), ( "c", [] ), ( "d", [] ) ]
-}
tuplesOfDatumAndFlatChildren : Tree a -> List ( a, List a )
tuplesOfDatumAndFlatChildren (Tree datum children) =
    [ ( datum, List.concatMap flatten children ) ] ++ (List.concatMap tuplesOfDatumAndFlatChildren children)


{-| Return the length of the Tree. Calculated recusively as datum (1) + length of children (n)
    Since a MultiwayTree is never empty this function will never return Int < 1.
-}
length : Tree a -> Int
length tree =
    foldr (\_ accu -> accu + 1) 0 tree


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


{-| Map a Function over a List and a MultiwayTree.
-}
mapListOverTree : (a -> b -> result) -> List a -> Tree b -> Maybe (Tree result)
mapListOverTree fn list (Tree datum children) =
    case list of
        [] ->
            Nothing

        head :: [] ->
            let
                mappedDatum =
                    fn head datum
            in
                Just (Tree mappedDatum [])

        head :: rest ->
            let
                mappedDatum =
                    fn head datum

                listGroupedByLengthOfChildren =
                    splitByLength (List.map length children) rest

                mappedChildren =
                    List.map2 (\l child -> mapListOverTree fn l child) listGroupedByLengthOfChildren children
                        |> List.filterMap identity
            in
                Just (Tree mappedDatum mappedChildren)


splitByLength : List Int -> List a -> List (List a)
splitByLength listOflengths list =
    splitByLength' listOflengths list []


splitByLength' : List Int -> List a -> List (List a) -> List (List a)
splitByLength' listOflengths list accu =
    case listOflengths of
        [] ->
            List.reverse accu

        currentLength :: restLengths ->
            case list of
                [] ->
                    List.reverse accu

                head :: rest ->
                    splitByLength' restLengths (List.drop currentLength list) ((List.take currentLength list) :: accu)


{-| Same as map but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> Tree a -> Maybe (Tree b)
indexedMap f tree =
    mapListOverTree f [0..(length tree - 1)] tree


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
