module Test.FlattenTests exposing (suite)

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
    describe "Flatten"
        [ test "Flatten multiChildTree" <|
            \_ ->
                Expect.equal
                    [ "a", "b", "c", "d" ]
                    (MultiwayTree.flatten multiChildTree)
        , test "Flatten deepTree" <|
            \_ ->
                Expect.equal
                    [ "a", "b", "c", "d" ]
                    (MultiwayTree.flatten deepTree)
        , test "Flatten interestingTree" <|
            \_ ->
                Expect.equal
                    [ "a", "b", "e", "k", "c", "f", "g", "d", "h", "i", "j" ]
                    (MultiwayTree.flatten interestingTree)
        ]
