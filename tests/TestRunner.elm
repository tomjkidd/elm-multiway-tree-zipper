module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (..)
import Task

import Tests

console : IO ()
console = consoleRunner <| Tests.all

port runner : Signal (Task.Task x ())
port runner = run console
