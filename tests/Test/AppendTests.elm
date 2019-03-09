module Test.AppendTests exposing (suite)

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
    describe "Append"
        [ test "appending children can turn a multiChildTree into an interestingTree" <|
            \_ ->
                Expect.equal
                    (Just ( interestingTree, [] ))
                    (Just ( multiChildTree, [] )
                        |> andThen (goToChild 0)
                        |> andThen (appendChild (Tree "e" []))
                        |> andThen (goToChild 0)
                        |> andThen (appendChild (Tree "k" []))
                        |> andThen goUp
                        |> andThen goRight
                        |> andThen (appendChild (Tree "f" []))
                        |> andThen (appendChild (Tree "g" []))
                        |> andThen goRight
                        |> andThen (appendChild (Tree "h" []))
                        |> andThen (appendChild (Tree "i" []))
                        |> andThen (appendChild (Tree "j" []))
                        |> andThen goToRoot
                    )
        ]
