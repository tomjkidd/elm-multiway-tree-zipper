module Test.InsertTests exposing (..)

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
    suite "Insert"
        [ test "Inserting children can turn a multiChildTree into an interestingTree"
            <| assertEqual (Just ( interestingTree, [] ))
                (Just ( multiChildTree, [] )
                    &> goToChild 0
                    &> insertChild (Tree "e" [])
                    &> goToChild 0
                    &> insertChild (Tree "k" [])
                    &> goUp
                    &> goRight
                    &> insertChild (Tree "g" [])
                    &> insertChild (Tree "f" [])
                    &> goRight
                    &> insertChild (Tree "j" [])
                    &> insertChild (Tree "i" [])
                    &> insertChild (Tree "h" [])
                    &> goToRoot
                )
        ]
