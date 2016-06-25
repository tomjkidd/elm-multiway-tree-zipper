module Test.IndexedMapTests exposing (..)

import ElmTest exposing (..)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper exposing (..)
import Test.SampleData
    exposing
        ( noChildTree
        , singleChildTree
        , multiChildTree
        , deepTree
        , noChildRecord
        , interestingTree
        )


tests : Test
tests =
    suite "IndexedMap"
        [ test "Maps a function with index over the Tree, transforms Tree"
            <| assertEqual [0..10]
                (case MultiwayTree.indexedMap (\index c -> index) interestingTree of
                    Just tree ->
                        (MultiwayTree.flatten tree)

                    Nothing ->
                        []
                )
        ]
