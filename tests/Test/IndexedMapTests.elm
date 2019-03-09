module Test.IndexedMapTests exposing (suite)

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
    describe "IndexedMap"
        [ test "Maps a function with index over the Tree, transforms Tree" <|
            \_ ->
                Expect.equal
                    (List.range 0 10)
                    (case MultiwayTree.indexedMap (\index c -> index) interestingTree of
                        Just tree ->
                            MultiwayTree.flatten tree

                        Nothing ->
                            []
                    )
        ]
