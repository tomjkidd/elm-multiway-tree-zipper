module MultiwayTreeZipper exposing
    ( Context(..), Breadcrumbs, Zipper
    , goToChild, goUp, goToRoot, goLeft, goRight, goToNext, goToPrevious, goToRightMostChild, goTo
    , updateDatum, replaceDatum, insertChild, appendChild, updateChildren
    , datum, maybeDatum
    )

{-| A library for navigating and updating immutable trees. The elements in
the tree must have the same type. The trees are implemented in a Huet
Zipper fashion.


# Types

@docs Context, Breadcrumbs, Zipper


# Navigation API

@docs goToChild, goUp, goToRoot, goLeft, goRight, goToNext, goToPrevious, goToRightMostChild, goTo


# Update API

@docs updateDatum, replaceDatum, insertChild, appendChild, updateChildren


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
import MultiwayTree exposing (Forest, Tree(..), appendChild, children, insertChild)


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

  - before: The list of children that come before the desired child
  - focus: The desired child Tree
  - after: The list of children that come after the desired child

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

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 0)
        |> andThen goUp

-}
goUp : Zipper a -> Maybe (Zipper a)
goUp ( tree, breadcrumbs ) =
    case breadcrumbs of
        (Context d before after) :: bs ->
            Just ( Tree d (before ++ [ tree ] ++ after), bs )

        [] ->
            Nothing


{-| Move down relative to the current Zipper focus. This allows navigation from
a parent to it's children.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 1)

-}
goToChild : Int -> Zipper a -> Maybe (Zipper a)
goToChild n ( Tree d c, breadcrumbs ) =
    let
        maybeSplit =
            splitOnIndex n c
    in
    case maybeSplit of
        Nothing ->
            Nothing

        Just ( before, focus, after ) ->
            Just ( focus, Context d before after :: breadcrumbs )


{-| Move down and as far right as possible relative to the current Zipper focus.
This allows navigation from a parent to it's last child.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToRightMostChild)

-}
goToRightMostChild : Zipper a -> Maybe (Zipper a)
goToRightMostChild ( Tree d c, breadcrumbs ) =
    goToChild (List.length c - 1) ( Tree d c, breadcrumbs )


{-| Move left relative to the current Zipper focus. This allows navigation from
a child to it's previous sibling.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 1)
        |> andThen (goLeft)

-}
goLeft : Zipper a -> Maybe (Zipper a)
goLeft ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Nothing

        (Context d before after) :: bs ->
            case List.reverse before of
                [] ->
                    Nothing

                tree_ :: rest ->
                    Just ( tree_, Context d (List.reverse rest) (tree :: after) :: bs )


{-| Move right relative to the current Zipper focus. This allows navigation from
a child to it's next sibling.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 1)
        |> andThen (goRight)

-}
goRight : Zipper a -> Maybe (Zipper a)
goRight ( tree, breadcrumbs ) =
    case breadcrumbs of
        (Context d before after) :: bs ->
            case after of
                [] ->
                    Nothing

                (Tree nextDatum nextChildren) :: rest ->
                    Just ( Tree nextDatum nextChildren, Context d (before ++ [ tree ]) rest :: bs )

        [] ->
            Nothing


{-| Moves to the previous node in the hierarchy, depth-first.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 2)
        |> andThen (goToPrevious)
        |> andThen (goToPrevious)

-}
goToPrevious : Zipper a -> Maybe (Zipper a)
goToPrevious zipper =
    let
        recurseDownAndRight zipper_ =
            case goToRightMostChild zipper_ of
                Just zipper__ ->
                    recurseDownAndRight zipper__

                Nothing ->
                    Just zipper_
    in
    case goLeft zipper of
        Just zipper_ ->
            recurseDownAndRight zipper_

        Nothing ->
            goUp zipper


{-| Moves to the next node in the hierarchy, depth-first. If already
at the end, stays there.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b" []
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToNext)
        |> andThen (goToNext)

-}
goToNext : Zipper a -> Maybe (Zipper a)
goToNext zipper =
    let
        upAndOver z =
            case goUp z of
                Nothing ->
                    Nothing

                Just z1 ->
                    case goRight z1 of
                        Nothing ->
                            upAndOver z1

                        z2 ->
                            z2
    in
    case goToChild 0 zipper of
        Just zipper_ ->
            Just zipper_

        Nothing ->
            case goRight zipper of
                Just zipper_ ->
                    Just zipper_

                Nothing ->
                    case upAndOver zipper of
                        Nothing ->
                            Nothing

                        zipper_ ->
                            zipper_


{-| Move to the root of the current Zipper focus. This allows navigation from
any part of the tree back to the root.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 0)
        |> andThen (goToChild 1)
        |> andThen (goToRoot)

-}
goToRoot : Zipper a -> Maybe (Zipper a)
goToRoot ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            Just ( tree, breadcrumbs )

        otherwise ->
            goUp ( tree, breadcrumbs ) |> Maybe.andThen goToRoot


{-| Move the focus to the first element for which the predicate is True. If no
such element exists returns Nothing. Starts searching at the root of the tree.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goTo (\elem -> elem == "e"))

-}
goTo : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
goTo predicate zipper =
    let
        goToElementOrNext ( Tree d c, breadcrumbs ) =
            if predicate d then
                Just ( Tree d c, breadcrumbs )

            else
                goToNext ( Tree d c, breadcrumbs ) |> Maybe.andThen goToElementOrNext
    in
    goToRoot zipper |> Maybe.andThen goToElementOrNext


{-| Update the datum at the current Zipper focus. This allows changes to be made
to a part of a node's datum information, given the previous state of the node.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 0)
        |> andThen (updateDatum (\old -> old ++ "X")) -- Appends an X to "b"
        |> andThen (goToRoot)

-}
updateDatum : (a -> a) -> Zipper a -> Maybe (Zipper a)
updateDatum fn ( Tree d c, breadcrumbs ) =
    Just ( Tree (fn d) c, breadcrumbs )


{-| Replace the datum at the current Zipper focus. This allows complete
replacement of a node's datum information, ignoring the previous state of the
node.

    import Maybe exposing (andThen)

    simpleTree =
        Tree "a"
            [ Tree "b"
                [ Tree "e" [] ]
            , Tree "c" []
            , Tree "d" []
            ]

    Just (simpleTree, [])
        |> andThen (goToChild 0)
        |> andThen (replaceDatum "X") -- Replaces "b" with "X"
        |> andThen (goToRoot)

-}
replaceDatum : a -> Zipper a -> Maybe (Zipper a)
replaceDatum newDatum =
    updateDatum (\_ -> newDatum)


{-| Fully replace the children at the current Zipper focus.
-}
updateChildren : Forest a -> Zipper a -> Maybe (Zipper a)
updateChildren newChildren ( Tree d c, breadcrumbs ) =
    Just ( Tree d newChildren, breadcrumbs )


{-| Inserts a Tree as the first child of the Tree at the current focus. Does not move the focus.
-}
insertChild : Tree a -> Zipper a -> Maybe (Zipper a)
insertChild child ( tree, breadcrumbs ) =
    Just ( MultiwayTree.insertChild child tree, breadcrumbs )


{-| Inserts a Tree as the last child of the Tree at the current focus. Does not move the focus.
-}
appendChild : Tree a -> Zipper a -> Maybe (Zipper a)
appendChild child ( tree, breadcrumbs ) =
    Just ( MultiwayTree.appendChild child tree, breadcrumbs )


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
