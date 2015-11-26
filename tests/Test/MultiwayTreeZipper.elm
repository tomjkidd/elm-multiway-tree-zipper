module Test.MultiwayTreeZipper (tests) where

import ElmTest exposing (..)

import MultiwayTree exposing (Tree (..))
import MultiwayTreeZipper exposing (..)

import Test.SampleData exposing (singleChildTree, multiChildTree, deepTree)
import Test.NavigationTests
import Test.UpdateTests

(&>) = Maybe.andThen

tests : Test
tests =
    suite "MultiwayTreeZipper"
        [ Test.NavigationTests.tests
        , Test.UpdateTests.tests
        ]
