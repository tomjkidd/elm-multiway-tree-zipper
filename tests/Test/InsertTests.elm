module Test.InsertTests exposing (suite)

import Expect exposing (Expectation)
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
        , singleChildTree
        )


suite : Test
suite =
    describe "Insert"
        [ test "Inserting children can turn a multiChildTree into an interestingTree" <|
            \_ ->
                Expect.equal (Just ( interestingTree, [] ))
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (insertChild (Tree "e" []))
                        |> andThen (goToChild 0)
                        |> andThen (insertChild (Tree "k" []))
                        |> andThen goUp
                        |> andThen goRight
                        |> andThen (insertChild (Tree "g" []))
                        |> andThen (insertChild (Tree "f" []))
                        |> andThen goRight
                        |> andThen (insertChild (Tree "j" []))
                        |> andThen (insertChild (Tree "i" []))
                        |> andThen (insertChild (Tree "h" []))
                        |> andThen goToRoot
                    )
        ]
