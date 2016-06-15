module Tests exposing (..)

import ElmTest exposing (..)

import Test.MultiwayTreeZipper as MultiwayTreeZipper

all : Test
all =
    suite "Elm MultiwayTreeZipper Tests"
        [ MultiwayTreeZipper.tests
        ]

main : Program Never
main = runSuite all
