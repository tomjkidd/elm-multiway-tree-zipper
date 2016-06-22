module MultiwayTreeZipper
    exposing
        ( Context(..)
        , Breadcrumbs
        , Zipper
        , goToChild
        , goToRightMostChild
        , goUp
        , goLeft
        , goRight
        , goToRoot
        , goToNext
        , goToPrevious
        , updateDatum
        , replaceDatum
        , datum
        , maybeDatum
        )

{-| A library for navigating and updating immutable trees. The elements in
the tree must have the same type. The trees are implemented in a Huet
Zipper fashion.

# Types
@docs Context, Breadcrumbs, Zipper

# Navigation API
@docs goToChild, goUp, goToRoot, goLeft, goRight, goToNext, goToPrevious, goToRightMostChild

# Update API
@docs updateDatum, replaceDatum

# Access API
@docs datum, maybeDatum


# References
[The Zipper, Gerard Huet](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
[Learn You A Haskell, Zippers, Miran Lipovaca](http://learnyouahaskell.com/zippers)

# Future work
Might be able to integrate existing [Rose Tree](http://package.elm-lang.org/packages/TheSeamau5/elm-rosetree) to work with the Zipper.
Wanted the first version to be self contained.

-}

-- TODO: Add more documentation

import List
import Maybe exposing (Maybe(..))
import MultiwayTree exposing (Tree(..), Forest, children)


{-| The necessary information needed to reconstruct a MultiwayTree as it is
navigated with a Zipper. This context includes the datum that was at the
previous node, a list of children that came before the node, and a list of
children that came after the node.
-}
type Context a
    = Context a (List (Tree a)) (List (Tree a))


{-| A list of Contexts that is contructed as a MultiwayTree is navigated.
Breadcrumbs are used to retain information about parts of the tree that move out
of focus. As the tree is navigated, the needed Context is pushed onto the list
Breadcrumbs, and they are maintained in the reverse order in which they are
visited
-}
type alias Breadcrumbs a =
    List (Context a)


{-| A structure to keep track of the current Tree, as well as the Breadcrumbs to
allow us to continue navigation through the rest of the tree.
-}
type alias Zipper a =
    ( Tree a, Breadcrumbs a )


{-| Separate a list into three groups. This function is unique to MultiwayTree
needs. In order to navigate to children of any Tree, a way to break the children
into pieces is needed.

The pieces are:
* before: The list of children that come before the desired child
* focus: The desired child Tree
* after: The list of children that come after the desired child

These pieces help create a Context, which assist the Zipper
-}
splitOnIndex : Int -> List (Tree a) -> Maybe ( List (Tree a), Tree a, List (Tree a) )
splitOnIndex n xs =
    let
        before =
            List.take n xs

        focus =
            List.drop n xs |> List.head

        after =
            List.drop (n + 1) xs
    in
        case focus of
            Nothing ->
                Nothing

            Just f ->
                Just ( before, f, after )


{-| Move up relative to the current Zipper focus. This allows navigation from a
child to it's parent.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 0
        &> goUp
-}
goUp : Zipper a -> Maybe (Zipper a)
goUp ( tree, breadcrumbs ) =
    case breadcrumbs of
        (Context datum before after) :: bs ->
            Just ( Tree datum (before ++ [ tree ] ++ after), bs )

        [] ->
            Nothing


{-| Move down relative to the current Zipper focus. This allows navigation from
a parent to it's children.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 1
-}
goToChild : Int -> Zipper a -> Maybe (Zipper a)
goToChild n ( Tree datum children, breadcrumbs ) =
    let
        maybeSplit =
            splitOnIndex n children
    in
        case maybeSplit of
            Nothing ->
                Nothing

            Just ( before, focus, after ) ->
                Just ( focus, (Context datum before after) :: breadcrumbs )


{-| Move down and as far right as possible  relative to the current Zipper focus. 
This allows navigation from a parent to it's last child.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToRightMostChild
-}
goToRightMostChild : Zipper a -> Maybe (Zipper a)
goToRightMostChild ( Tree datum children, breadcrumbs ) =
    goToChild ((List.length children) - 1) ( Tree datum children, breadcrumbs )


{-| Move left relative to the current Zipper focus. This allows navigation from
a child to it's previous sibling.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 1
        &> goLeft
-}
goLeft : Zipper a -> Maybe (Zipper a)
goLeft ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Nothing

        (Context datum before after) :: bs ->
            case List.reverse before of
                [] ->
                    Nothing

                tree' :: rest ->
                    Just ( tree', (Context datum (List.reverse rest) (tree :: after)) :: bs )


{-| Move right relative to the current Zipper focus. This allows navigation from
a child to it's next sibling.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 1
        &> goRight
-}
goRight : Zipper a -> Maybe (Zipper a)
goRight ( tree, breadcrumbs ) =
    case breadcrumbs of
        (Context datum before after) :: bs ->
            case after of
                [] ->
                    Nothing

                (Tree nextDatum nextChildren) :: rest ->
                    Just ( (Tree nextDatum nextChildren), (Context datum (before ++ [ tree ]) rest) :: bs )

        [] ->
            Nothing


{-| Moves to the previous node in the hierarchy, depth-first.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 2
        &> goToPrevious
        &> goToPrevious
-}
goToPrevious : Zipper a -> Maybe (Zipper a)
goToPrevious zipper =
    let
        recurseDownAndRight zipper' =
            case goToRightMostChild zipper' of
                Just zipper'' ->
                    recurseDownAndRight zipper''

                Nothing ->
                    Just zipper'
    in
        case goLeft zipper of
            Just zipper' ->
                recurseDownAndRight zipper'

            Nothing ->
                goUp zipper


{-| Moves to the next node in the hierarchy, depth-first. If already
  at the end, stays there.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToNext
        &> goToNext
-}
goToNext : Zipper a -> Maybe (Zipper a)
goToNext zipper =
    let
        upAndOver zipper =
            case goUp zipper of
                Nothing ->
                    Nothing

                Just zipper' ->
                    case goRight zipper' of
                        Nothing ->
                            upAndOver zipper'

                        zipper'' ->
                            zipper''
    in
        case goToChild 0 zipper of
            Just zipper' ->
                Just zipper'

            Nothing ->
                case goRight zipper of
                    Just zipper' ->
                        Just zipper'

                    Nothing ->
                        case upAndOver zipper of
                            Nothing ->
                                Nothing

                            zipper' ->
                                zipper'


{-| Move to the root of the current Zipper focus. This allows navigation from
any part of the tree back to the root.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 0
        &> goToChild 1
        &> goToRoot
-}
goToRoot : Zipper a -> Maybe (Zipper a)
goToRoot ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Just ( tree, breadcrumbs )

        otherwise ->
            goUp ( tree, breadcrumbs ) `Maybe.andThen` goToRoot


{-| Update the datum at the current Zipper focus. This allows changes to be made
to a part of a node's datum information, given the previous state of the node.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 0
        &> updateDatum (\old -> old ++ "X") -- Appends an X to "b"
        &> goToRoot
-}
updateDatum : (a -> a) -> Zipper a -> Maybe (Zipper a)
updateDatum fn ( Tree datum children, breadcrumbs ) =
    Just ( Tree (fn datum) children, breadcrumbs )


{-| Replace the datum at the current Zipper focus. This allows complete
replacement of a node's datum information, ignoring the previous state of the
node.

    (&>) = Maybe.andThen

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        &> goToChild 0
        &> replaceDatum "X" -- Replaces "b" with "X"
        &> goToRoot
-}
replaceDatum : a -> Zipper a -> Maybe (Zipper a)
replaceDatum newDatum =
    updateDatum (\_ -> newDatum)


{-| Fully replace the children at the current Zipper focus.
-}
updateChildren : Forest a -> Zipper a -> Zipper a
updateChildren newChildren ( Tree datum children, breadcrumbs ) =
    ( Tree datum newChildren, breadcrumbs )


{-| Access the datum at the current Zipper focus.
-}
datum : Zipper a -> a
datum ( tree, breadcrumbs ) =
    MultiwayTree.datum tree


{-| Access the datum at the current Zipper focus as a Maybe.
-}
maybeDatum : Zipper a -> Maybe a
maybeDatum zipper =
    datum zipper
        |> Just
