module Test.LengthTests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
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
    suite "Length"
        [ test "Length of an interesting Tree" <|
            assertEqual 11
                (MultiwayTree.length interestingTree)
        , test "Length of a noChildTree" <|
            assertEqual 1
                (MultiwayTree.length noChildTree)
        , test "Length of a deepTree" <|
            assertEqual 4
                (MultiwayTree.length deepTree)
        , test "Length of a Tree is equal to length of a flattened tree" <|
            assertEqual (List.length (MultiwayTree.flatten interestingTree))
                (MultiwayTree.length interestingTree)
        ]
