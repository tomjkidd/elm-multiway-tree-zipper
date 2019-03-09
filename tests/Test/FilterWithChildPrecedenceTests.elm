module Test.FilterWithChildPrecedenceTests exposing (suite)

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
                    (MultiwayTree.filterWithChildPrecedence (\_ -> True) interestingTree)
        , test "Filtering a Tree with a predicate returns a filtered Tree" <|
            \_ ->
                Expect.equal
                    (Just multiChildTree)
                    (MultiwayTree.filterWithChildPrecedence (\e -> e < "e") interestingTree)
        , test "If an element is no where to be found in the tree returns Nothing" <|
            \_ ->
                Expect.equal
                    Nothing
                    (MultiwayTree.filterWithChildPrecedence (\e -> e == "fooo") interestingTree)
        , test "If a predicate evaluates to False for a Node but True for one of it's children then the Node will remain in the Tree" <|
            \_ ->
                Expect.equal
                    (Just
                        (Tree "a"
                            [ Tree "b"
                                [ Tree "e"
                                    [ Tree "k" [] ]
                                ]
                            ]
                        )
                    )
                    (MultiwayTree.filterWithChildPrecedence (\e -> e == "k") interestingTree)
        ]
