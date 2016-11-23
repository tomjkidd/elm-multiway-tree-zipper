module Test.FoldTests exposing (..)

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
    suite "Fold"
        [ test "Foldl interestingTree into List"
            <| assertEqual (MultiwayTree.flatten interestingTree)
                ((MultiwayTree.foldl (::) [] interestingTree) |> List.reverse)
        ]
