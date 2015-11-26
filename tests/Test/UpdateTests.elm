module Test.UpdateTests where

import ElmTest exposing (..)

import MultiwayTree exposing (Tree (..))
import MultiwayTreeZipper exposing (..)
import Test.SampleData exposing
    ( noChildTree, singleChildTree, multiChildTree, deepTree
    , noChildRecord )

(&>) = Maybe.andThen

tests : Test
tests =
    suite "Update"
          [ test "Update datum (simple)"
              <| assertEqual
                  (Just (( Tree "ax" [] ), []))
                  (Just (noChildTree, [])
                    &> updateDatum (\a -> a ++ "x"))

          , test "Update datum (record)"
              <| assertEqual
                  (Just (( Tree { selected = True, expanded = False } [] ), []))
                  (Just (noChildRecord, [])
                    &> updateDatum (\rec -> { rec | selected = True } ))

          , test "Replace datum (simple)"
              <| assertEqual
                  (Just (( Tree "x" [] ), []))
                  (Just (noChildTree, [])
                    &> replaceDatum "x" )

          , test "Replace datum (record)"
              <| assertEqual
                  (Just (( Tree { selected = True, expanded = True } [] ), []))
                  (Just (noChildRecord, [])
                    &> replaceDatum { selected = True, expanded = True } )

          ]
