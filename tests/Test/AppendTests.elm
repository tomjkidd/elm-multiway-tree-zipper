module Test.AppendTests exposing (..)

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


(&>) =
    Maybe.andThen


tests : Test
tests =
    suite "Append"
        [ test "appending children can turn a multiChildTree into an interestingTree"
            <| assertEqual (Just ( interestingTree, [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild 0
                    &> appendChild (Tree "e" [])
                    &> goToChild 0
                    &> appendChild (Tree "k" [])
                    &> goUp
                    &> goRight
                    &> appendChild (Tree "f" [])
                    &> appendChild (Tree "g" [])
                    &> goRight
                    &> appendChild (Tree "h" [])
                    &> appendChild (Tree "i" [])
                    &> appendChild (Tree "j" [])
                    &> goToRoot
                )
        ]
