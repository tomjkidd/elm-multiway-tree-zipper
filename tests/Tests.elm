module Tests exposing (..)

import Legacy.ElmTest as ElmTest exposing (..)
import Test.MultiwayTreeZipper as MultiwayTreeZipper


all : Test
all =
    suite "Elm MultiwayTreeZipper Tests"
        [ MultiwayTreeZipper.tests
        ]


main : Program Never () msg
main =
    runSuite all
