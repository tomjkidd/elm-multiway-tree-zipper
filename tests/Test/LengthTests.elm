module Test.LengthTests exposing (suite)

import Expect exposing (Expectation)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper exposing (..)
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


suite : Test
suite =
    describe "Length"
        [ test "Length of an interesting Tree" <|
            \_ ->
                Expect.equal
                    11
                    (MultiwayTree.length interestingTree)
        , test "Length of a noChildTree" <|
            \_ ->
                Expect.equal
                    1
                    (MultiwayTree.length noChildTree)
        , test "Length of a deepTree" <|
            \_ ->
                Expect.equal
                    4
                    (MultiwayTree.length deepTree)
        , test "Length of a Tree is equal to length of a flattened tree" <|
            \_ ->
                Expect.equal
                    (List.length (MultiwayTree.flatten interestingTree))
                    (MultiwayTree.length interestingTree)
        ]
