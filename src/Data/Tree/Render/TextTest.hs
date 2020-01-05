{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

module Data.Tree.Render.TextTest (
  test1,
  test2,
  test3,
) where

import qualified Data.Tree as Tree
import           Data.Tree ( Tree )
import qualified Data.Tree.Render.Text as R
import qualified Text.PrettyPrint.Boxes as Box
import           Text.PrettyPrint.Boxes ( Box )

naturalBox :: String -> Box
naturalBox = Box.vcat Box.left . map Box.text . lines

vsep :: [Box] -> Box
vsep = Box.vsep 2 Box.left

hsep :: [Box] -> Box
hsep = Box.hsep 3 Box.left

renderFlavors :: R.RenderOptions String String -> Box
renderFlavors options =
  let go ord loc =
        let str = flip R.renderTree testTree1 options
              { R.oChildOrder = const $ pure ord
              , R.oParentLocation = const $ pure loc
              }
        in naturalBox str
  in hsep
    [ go R.FirstToLast R.ParentBeforeChildren
    , go R.FirstToLast R.ParentAfterChildren
    , go R.FirstToLast R.ParentBetweenChildren
    , go R.LastToFirst R.ParentBeforeChildren
    , go R.LastToFirst R.ParentAfterChildren
    , go R.LastToFirst R.ParentBetweenChildren
    , go R.LastToFirst $ R.ParentAtChildIndex 1
    , go R.FirstToLast $ R.ParentAtChildIndex 1
    ]

test1 :: IO ()
test1 = do
  let options = R.tracedRenderOptions id
  putStrLn ""
  let b0 = renderFlavors options
  let b1 = renderFlavors options { R.oVerticalPad = 1 }
  Box.printBox $ vsep [b0, b1]
  putStrLn ""

test2 :: IO ()
test2 = do
  let tree = testTree1
  let options = (R.tracedRenderOptions id)
        { R.oChildOrder = const $ pure R.LastToFirst
        , R.oParentLocation = const $ pure R.ParentBetweenChildren
        }
  putStrLn ""
  let f0 = R.renderForest options []
  let f1 = R.renderForest options [tree]
  let f2 = R.renderForest options [tree, tree]
  Box.printBox $ hsep $ map naturalBox [f0, f1, f2]
  putStrLn ""

test3 :: IO ()
test3 = do
  let tree = testTree2
  let options = (R.zigZagRenderOptions id)
  putStrLn ""
  let f1 = R.renderForest options [tree]
  Box.printBox $ hsep $ map naturalBox [f1]
  putStrLn ""

testTree1 :: Tree String
testTree1
  = node "Add"
    [ node "Add"
      [ node "0" []
      , node "Mul"
        [ node "1" []
        , node "2" []
        ]
      ]
    , node "Neg"
      [ node "Max"
        [ node "3" []
        , node "4" []
        , node "5" []
        , node "Var"
          [ node "x" []
          ]
        , node "6" []
        ]
      ]
    ]

  where
    node :: String -> [Tree String] -> Tree String
    node = Tree.Node

testTree2 :: Tree String
testTree2
  = node "Add"
    [ node "Add"
      [ node "a" []
      , node "Mul"
        [ node "b" [testTree1]
        , node "c" []
        ]
      ]
    , node "Neg"
      [ node "Max"
        [ node "d" []
        , node "e" []
        , node "f" []
        , node "Var"
          [ node "x" [testTree1]
          ]
        , node "g" []
        ]
      ]
    ]

  where
    node :: String -> [Tree String] -> Tree String
    node = Tree.Node

