module Test.FoldTests exposing (suite)

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
    describe "Fold"
        [ test "Foldl interestingTree into List" <|
            \_ ->
                Expect.equal
                    (MultiwayTree.flatten interestingTree)
                    (MultiwayTree.foldl (::) [] interestingTree |> List.reverse)
        ]
