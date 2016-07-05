module Test.MultiwayTreeZipper exposing (tests)

import ElmTest exposing (..)
import Test.NavigationTests
import Test.UpdateTests
import Test.FlattenTests
import Test.FoldTests
import Test.FilterTests
import Test.FilterWithChildPrecedenceTests
import Test.AppendTests
import Test.InsertTests
import Test.LengthTests
import Test.IndexedMapTests
import Test.TuplesOfDatumAndFlatChildrenTests
import Test.SortTests


tests : Test
tests =
    suite "MultiwayTreeZipper"
        [ Test.NavigationTests.tests
        , Test.UpdateTests.tests
        , Test.FlattenTests.tests
        , Test.FoldTests.tests
        , Test.FilterTests.tests
        , Test.FilterWithChildPrecedenceTests.tests
        , Test.AppendTests.tests
        , Test.InsertTests.tests
        , Test.LengthTests.tests
        , Test.IndexedMapTests.tests
        , Test.TuplesOfDatumAndFlatChildrenTests.tests
        , Test.SortTests.tests
        ]
