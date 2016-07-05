module Test.SortTests exposing (..)

import ElmTest exposing (..)
import MultiwayTree exposing (Tree(..))
import Test.SampleData
    exposing
        ( noChildTree
        , singleChildTree
        , multiChildTree
        , deepTree
        , noChildRecord
        , interestingTree
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


tests : Test
tests =
    suite "Sort"
        [ test "Sorting a Tree with only one child per levels yields the same Tree"
            <| assertEqual deepTree
                (MultiwayTree.sortBy identity deepTree)
        , test "Sorting a sorted Tree returns the same Tree"
            <| assertEqual interestingTree
                (MultiwayTree.sortBy identity interestingTree)
        , test "Sorting an unsorted Tree returns a sorted Tree"
            <| assertEqual interestingTree
                (MultiwayTree.sortBy identity unorderedTree)
        , test "Sorting with a Tree with a reversed comperator reverse-sorts a Tree"
            <| assertEqual reverseSortedTree
                (MultiwayTree.sortWith flippedComparison interestingTree)
        ]
