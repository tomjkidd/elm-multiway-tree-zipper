module Test.TuplesOfDatumAndFlatChildrenTests exposing (suite)

import Expect
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
    describe "TuplesOfDatumAndFlatChildren"
        [ test "TuplesOfDatumAndFlatChildren multiChildTree" <|
            \_ ->
                Expect.equal
                    [ ( "a", [ "b", "c", "d" ] ), ( "b", [] ), ( "c", [] ), ( "d", [] ) ]
                    (MultiwayTree.tuplesOfDatumAndFlatChildren multiChildTree)
        , test "TuplesOfDatumAndFlatChildren deepTree" <|
            \_ ->
                Expect.equal
                    [ ( "a", [ "b", "c", "d" ] ), ( "b", [ "c", "d" ] ), ( "c", [ "d" ] ), ( "d", [] ) ]
                    (MultiwayTree.tuplesOfDatumAndFlatChildren deepTree)
        , test "TuplesOfDatumAndFlatChildren interestingTree" <|
            \_ ->
                Expect.equal
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
