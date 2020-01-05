{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

module Data.Tree.Render.TextTest (
  test1,
  test2,
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
              { R.oChildOrder = ord
              , R.oParentLocation = loc
              }
        in naturalBox str
  in hsep
    [ go R.FirstToLast R.ParentBeforeChildren
    , go R.FirstToLast R.ParentAfterChildren
    , go R.FirstToLast R.ParentBetweenChildren
    , go R.LastToFirst R.ParentBeforeChildren
    , go R.LastToFirst R.ParentAfterChildren
    , go R.LastToFirst R.ParentBetweenChildren
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
  let options = (R.tracedRenderOptions id)
        { R.oChildOrder = R.LastToFirst
        , R.oParentLocation = R.ParentBetweenChildren
        }
  let forest = [testTree1, testTree1]
  putStrLn ""
  let f1 = R.renderForest options forest
  putStrLn f1
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

