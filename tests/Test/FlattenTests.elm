module Test.FlattenTests exposing (..)

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
    suite "Flatten"
        [ test "Flatten multiChildTree"
            <| assertEqual [ "a", "b", "c", "d" ]
                (MultiwayTree.flatten multiChildTree)
        , test "Flatten deepTree"
            <| assertEqual [ "a", "b", "c", "d" ]
                (MultiwayTree.flatten deepTree)
        , test "Flatten interestingTree"
            <| assertEqual [ "a", "b", "e", "k", "c", "f", "g", "d", "h", "i", "j" ]
                (MultiwayTree.flatten interestingTree)
        ]
