module Test.TuplesOfDatumAndFlatChildrenTests exposing (..)

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
    suite "TuplesOfDatumAndFlatChildren"
        [ test "TuplesOfDatumAndFlatChildren multiChildTree"
            <| assertEqual [ ( "a", [ "b", "c", "d" ] ), ( "b", [] ), ( "c", [] ), ( "d", [] ) ]
                (MultiwayTree.tuplesOfDatumAndFlatChildren multiChildTree)
        , test "TuplesOfDatumAndFlatChildren deepTree"
            <| assertEqual [ ( "a", [ "b", "c", "d" ] ), ( "b", [ "c", "d" ] ), ( "c", [ "d" ] ), ( "d", [] ) ]
                (MultiwayTree.tuplesOfDatumAndFlatChildren deepTree)
        , test "TuplesOfDatumAndFlatChildren interestingTree"
            <| assertEqual
                [ ( "a", [ "b", "e", "k", "c", "f", "g", "d", "h", "i", "j" ] )
                , ( "b", [ "e", "k" ] )
                , ( "e", [ "k" ] )
                , ( "k", [] )
                , ( "c", [ "f", "g" ] )
                , ( "f", [] )
                , ( "g", [] )
                , ( "d", [ "h", "i", "j" ] )
                , ( "h", [] )
                , ( "i", [] )
                , ( "j", [] )
                ]
                (MultiwayTree.tuplesOfDatumAndFlatChildren interestingTree)
        ]
