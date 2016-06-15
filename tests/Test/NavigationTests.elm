module Test.NavigationTests exposing (..)

import ElmTest exposing (..)

import MultiwayTree exposing (Tree (..))
import MultiwayTreeZipper exposing (..)
import Test.SampleData exposing (singleChildTree, multiChildTree, deepTree)

(&>) = Maybe.andThen

tests : Test
tests =
    suite "Navigation"
          [ test "Navigate to child (only child)"
              <| assertEqual
                (Just ((Tree "b" [] ), [ Context "a" [] [] ]))
                (Just (singleChildTree, [])
                    &> goToChild 0)

          , test "Navigate to child (one of many)"
              <| assertEqual
                (Just
                    ((Tree "c" [] ) ,
                     [ Context "a"
                        [ (Tree "b" []) ]
                        [ (Tree "d" []) ]
                     ])
                )
                (Just (multiChildTree, [])
                    &> goToChild 1)

          , test "Navigate to a child (deep)"
              <| assertEqual
                (Just ((Tree "d" [] ),
                [ Context "c" [] []
                , Context "b" [] []
                , Context "a" [] []
                ]))
                (Just (deepTree, [])
                    &> goToChild 0
                    &> goToChild 0
                    &> goToChild 0)

          , test "Navigate up (single level)"
              <| assertEqual
                (Just ((Tree "a" [ Tree "b" [] ]), [] ))
                (Just (singleChildTree, [])
                    &> goToChild 0
                    &> goUp)

          , test "Navigate up (single level with many children)"
              <| assertEqual
                (Just ((Tree "a" [ Tree "b" [], Tree "c" [], Tree "d" [] ]), [] ))
                (Just (multiChildTree, [])
                    &> goToChild 1
                    &> goUp)

          , test "Navigate up from a child (deep)"
              <| assertEqual
                (Just ((Tree "a" [ Tree "b" [ Tree "c" [ Tree "d" [] ]]]), []))
                (Just (deepTree, [])
                    &> goToChild 0
                    &> goToChild 0
                    &> goToChild 0
                    &> goUp
                    &> goUp
                    &> goUp)

          , test "Navigate beyond the tree (only child)"
              <| assertEqual
                Nothing
                (Just (singleChildTree, [])
                    &> goToChild 0
                    &> goToChild 0)

          , test "Navigate beyond the tree (up past root)"
              <| assertEqual
                Nothing
                (Just (singleChildTree, [])
                    &> goUp)
          ]
