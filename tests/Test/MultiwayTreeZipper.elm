module Test.MultiwayTreeZipper exposing (tests)

import ElmTest exposing (..)
import MultiwayTree exposing (Tree(..))
import MultiwayTreeZipper exposing (..)
import Test.SampleData exposing (singleChildTree, multiChildTree, deepTree)
import Test.NavigationTests
import Test.UpdateTests
import Test.FlattenTests
import Test.FoldTests
import Test.FilterTests
import Test.FilterWithChildPrecedenceTests


(&>) =
    Maybe.andThen


tests : Test
tests =
    suite "MultiwayTreeZipper"
        [ Test.NavigationTests.tests
        , Test.UpdateTests.tests
        , Test.FlattenTests.tests
        , Test.FoldTests.tests
        , Test.FilterTests.tests
        , Test.FilterWithChildPrecedenceTests.tests
        ]
