module Test.SortTests exposing (flippedComparison, reverseSortedTree, suite, unorderedTree)

import Expect
import MultiwayTree exposing (Tree(..))
import Test exposing (..)
import Test.SampleData
    exposing
        ( deepTree
        , interestingTree
        , multiChildTree
        , noChildRecord
        , noChildTree
        , singleChildTree
        )


unorderedTree : Tree String
unorderedTree =
    Tree "a"
        [ Tree "c"
            [ Tree "g" []
            , Tree "f" []
            ]
        , Tree "b"
            [ Tree "e"
                [ Tree "k" [] ]
            ]
        , Tree "d"
            [ Tree "i" []
            , Tree "h" []
            , Tree "j" []
            ]
        ]


reverseSortedTree : Tree String
reverseSortedTree =
    Tree "a"
        [ Tree "d"
            [ Tree "j" []
            , Tree "i" []
            , Tree "h" []
            ]
        , Tree "c"
            [ Tree "g" []
            , Tree "f" []
            ]
        , Tree "b"
            [ Tree "e"
                [ Tree "k" [] ]
            ]
        ]


flippedComparison : comparable -> comparable -> Order
flippedComparison a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


suite : Test
suite =
    describe "Sort"
        [ test "Sorting a Tree with only one child per levels yields the same Tree" <|
            \_ ->
                Expect.equal
                    deepTree
                    (MultiwayTree.sortBy identity deepTree)
        , test "Sorting a sorted Tree returns the same Tree" <|
            \_ ->
                Expect.equal
                    interestingTree
                    (MultiwayTree.sortBy identity interestingTree)
        , test "Sorting an unsorted Tree returns a sorted Tree" <|
            \_ ->
                Expect.equal
                    interestingTree
                    (MultiwayTree.sortBy identity unorderedTree)
        , test "Sorting with a Tree with a reversed comperator reverse-sorts a Tree" <|
            \_ ->
                Expect.equal
                    reverseSortedTree
                    (MultiwayTree.sortWith flippedComparison interestingTree)
        ]
