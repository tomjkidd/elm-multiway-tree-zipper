module Test.UpdateTests exposing (suite)

import Expect
import Maybe exposing (andThen)
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
        , simpleForest
        , singleChildTree
        )


suite : Test
suite =
    describe "Update"
        [ test "Update datum (simple)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree "ax" [], [] ))
                    (Just ( noChildTree, [] )
                        |> andThen (updateDatum (\a -> a ++ "x"))
                    )
        , test "Update datum (record)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree { selected = True, expanded = False } [], [] ))
                    (Just ( noChildRecord, [] )
                        |> andThen (updateDatum (\rec -> { rec | selected = True }))
                    )
        , test "Replace datum (simple)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree "x" [], [] ))
                    (Just ( noChildTree, [] )
                        |> andThen (replaceDatum "x")
                    )
        , test "Replace datum (record)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree { selected = True, expanded = True } [], [] ))
                    (Just ( noChildRecord, [] )
                        |> andThen (replaceDatum { selected = True, expanded = True })
                    )
        , test "Replace children (replace with empty)" <|
            \_ ->
                Expect.equal
                    (Just ( noChildTree, [] ))
                    (Just ( singleChildTree, [] )
                        |> andThen (updateChildren [])
                    )
        , test "Replace children (replace with specific)" <|
            \_ ->
                Expect.equal
                    (Just ( Tree "a" simpleForest, [] ))
                    (Just ( interestingTree, [] )
                        |> andThen (updateChildren simpleForest)
                    )
        ]
