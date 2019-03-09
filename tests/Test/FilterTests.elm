module Test.FilterTests exposing (suite)

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
    describe "Filter"
        [ test "Filtering a Tree with a predicate that always returns true returns the same tree" <|
            \_ ->
                Expect.equal
                    (Just interestingTree)
                    (MultiwayTree.filter (\_ -> True) interestingTree)
        , test "Filtering a Tree with a predicate returns a filtered Tree" <|
            \_ ->
                Expect.equal
                    (Just multiChildTree)
                    (MultiwayTree.filter (\e -> e < "e") interestingTree)
        , test "If a subtree contains an element which would evaluate the predicate to True it is still not in the result Tree if the parent datum evaluates to false" <|
            \_ ->
                Expect.equal
                    Nothing
                    (MultiwayTree.filter (\e -> e == "k") interestingTree)
        ]
