module Tests where

import Task

import Console exposing (..)
import ElmTest exposing (..)

import Test.MultiwayTreeZipper as MultiwayTreeZipper

all : Test
all =
    suite "Elm MultiwayTreeZipper Tests"
        [ MultiwayTreeZipper.tests
        ]
