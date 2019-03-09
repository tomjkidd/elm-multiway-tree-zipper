module Test.NavigationTests exposing (suite)

import Expect exposing (Expectation)
import Maybe exposing (andThen)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper exposing (..)
import Test exposing (..)
import Test.SampleData exposing (deepTree, interestingTree, multiChildTree, noChildTree, singleChildTree)


suite : Test
suite =
    describe "Navigation"
        [ test "Navigate to child (only child)" <|
            \_ ->
                Expect.equal (Just ( Tree "b" [], [ Context "a" [] [] ] ))
                    (Just ( singleChildTree, [] )
                        |> andThen (goToChild 0)
                    )
        , test "Navigate to child (one of many)" <|
            \_ ->
                Expect.equal
                    (Just
                        ( Tree "c" []
                        , [ Context "a"
                                [ Tree "b" [] ]
                                [ Tree "d" [] ]
                          ]
                        )
                    )
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 1)
                    )
        , test "Navigate to a child (deep)" <|
            \_ ->
                Expect.equal
                    (Just
                        ( Tree "d" []
                        , [ Context "c" [] []
                          , Context "b" [] []
                          , Context "a" [] []
                          ]
                        )
                    )
                    (Just ( deepTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                    )
        , test "Navigate to last child of an empty tree returns Nothing" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( noChildTree, [] )
                        |> andThen goToRightMostChild
                    )
        , test "Navigate to last child of a tree with just one child moves to that child" <|
            \_ ->
                Expect.equal
                    (Just ( singleChildTree, [] )
                        |> andThen (goToChild 0)
                    )
                    (Just ( singleChildTree, [] )
                        |> andThen goToRightMostChild
                    )
        , test "Navigate to last child of a tree with multiple children moves to the last child" <|
            \_ ->
                Expect.equal
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 2)
                    )
                    (Just ( multiChildTree, [] )
                        |> andThen goToRightMostChild
                    )
        , test "Navigate to last child of an interestingTree" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen (goToChild 2)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen goToRightMostChild
                        |> andThen goToRightMostChild
                    )
        , test "Navigate up (single level)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree "a" [ Tree "b" [] ], [] ))
                    (Just ( singleChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen goUp
                    )
        , test "Navigate up (single level with many children)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree "a" [ Tree "b" [], Tree "c" [], Tree "d" [] ], [] ))
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 1)
                        |> andThen goUp
                    )
        , test "Navigate up from a child (deep)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree "a" [ Tree "b" [ Tree "c" [ Tree "d" [] ] ] ], [] ))
                    (Just ( deepTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                        |> andThen goUp
                        |> andThen goUp
                        |> andThen goUp
                    )
        , test "Navigate beyond the tree (only child)" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( singleChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                    )
        , test "Navigate beyond the tree (up past root)" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( singleChildTree, [] )
                        |> andThen goUp
                    )
        , test "Navigate to left sibling on no child tree does not work" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( noChildTree, [] )
                        |> andThen goLeft
                    )
        , test "Navigate to left child" <|
            \_ ->
                Expect.equal
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen goRight
                    )
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen goLeft
                    )
        , test "Navigate to left child twice" <|
            \_ ->
                Expect.equal
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 0)
                    )
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen goLeft
                        |> andThen goLeft
                    )
        , test "Navigate to left child when there are no siblings left return Nothing" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen goLeft
                    )
        , test "Navigate to right sibling on no child tree does not work" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( noChildTree, [] )
                        |> andThen goRight
                    )
        , test "Navigate to right child" <|
            \_ ->
                Expect.equal
                    (Just
                        ( Tree "c" []
                        , [ Context "a"
                                [ Tree "b" [] ]
                                [ Tree "d" [] ]
                          ]
                        )
                    )
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen goRight
                    )
        , test "Navigate to right child twice" <|
            \_ ->
                Expect.equal
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 2)
                    )
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen goRight
                        |> andThen goRight
                    )
        , test "Navigate to right child when there are no siblings left return Nothing" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen goRight
                    )
        , test "Navigate to next child on Tree with just one node" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( noChildTree, [] )
                        |> andThen goToNext
                    )
        , test "Navigate to next child on an interesting tree will select the next node" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen goToNext
                    )
        , test "Navigate to next child when the end of a branch has been reached will perform backtracking until the next node down can be reached" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 1)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                        |> andThen goToNext
                    )
        , test "Navigating past the end of a Tree will return Nothing" <|
            \_ ->
                Expect.equal Nothing
                    (Just ( deepTree, [] )
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                    )
        , test "Consecutive goToNext on an interestingTree end up on the right Node" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen (goToChild 1)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                        |> andThen goToNext
                    )
        , test "Navigate to previous child when there are siblings will select the sibling" <|
            \_ ->
                Expect.equal
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 1)
                    )
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen goToPrevious
                    )
        , test "Navigate to previous child on an interesting tree will select the previous node" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 0)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                        |> andThen goToPrevious
                    )
        , test "Navigate to previous child when the beginning of a branch has been reached will perform backtracking until the next node down can be reached" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                        |> andThen (goToChild 0)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 1)
                        |> andThen goToPrevious
                    )
        , test "Navigating past the beginning of a Tree will return Nothing" <|
            \_ ->
                Expect.equal Nothing
                    (Just ( singleChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                    )
        , test "Consecutive goToPrevious on an interestingTree end up on the right Node" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 0)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen (goToChild 2)
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                        |> andThen goToPrevious
                    )
        , test "Trying to find a non existing element in a Tree returns Nothing" <|
            \_ ->
                Expect.equal
                    Nothing
                    (Just ( interestingTree, [] )
                        |> andThen (goTo (\elem -> elem == "FOO"))
                    )
        , test "Trying to find an existing element in a Tree moves the focus to this element" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] )
                        |> andThen (goToChild 2)
                        |> andThen (goToChild 0)
                    )
                    (Just ( interestingTree, [] )
                        |> andThen (goTo (\elem -> elem == "h"))
                    )
        ]
