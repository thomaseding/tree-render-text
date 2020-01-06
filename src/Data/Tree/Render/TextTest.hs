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
        let str = flip R.renderTree tree1 options
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
  let b0 = renderFlavors options
  let b1 = renderFlavors options { R.oVerticalPad = 1 }
  putStrLn ""
  Box.printBox $ vsep [b0, b1]
  putStrLn ""

test2 :: IO ()
test2 = do
  let tree = tree1
  let options = (R.tracedRenderOptions id)
        { R.oChildOrder = const $ pure R.LastToFirst
        , R.oParentLocation = const $ pure R.ParentBetweenChildren
        }
  let f0 = R.renderForest options []
  let f1 = R.renderForest options [tree]
  let f2 = R.renderForest options [tree, tree]
  putStrLn ""
  Box.printBox $ hsep $ map naturalBox [f0, f1, f2]
  putStrLn ""

test3 :: IO ()
test3 = do
  let options1 = (R.middleCutRenderOptions id)
  let options2 = (R.zigZagRenderOptions id)
  let mkBox options =
        let f2 = R.renderForest options [tree2]
            f3 = R.renderForest options [tree3]
            f4 = R.renderForest options [tree4]
        in hsep $ map naturalBox [f2, f3, f4]
  putStrLn ""
  Box.printBox $ hsep $ map mkBox [options1, options2]
  putStrLn ""

tree1 :: Tree String
tree1
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

tree2 :: Tree String
tree2
  = node "Add"
    [ node "Add"
      [ node "a" []
      , node "Mul"
        [ node "b" [tree1]
        , node "c" []
        ]
      ]
    , node "Neg"
      [ node "Max"
        [ node "d" []
        , node "e" []
        , node "f" []
        , node "Var"
          [ node "x" [tree1]
          ]
        , node "g" []
        ]
      ]
    ]

  where
    node :: String -> [Tree String] -> Tree String
    node = Tree.Node

treeNat :: Int -> Tree Int
treeNat n = iterate s z !! n
  where
    z :: Tree Int
    z = Tree.Node 0 []

    s :: Tree Int -> Tree Int
    s t = Tree.Node (1 + Tree.rootLabel t) [t]

treePow2 :: Int -> Tree Int
treePow2 n = iterate f z !! n
  where
    z :: Tree Int
    z = Tree.Node 1 []

    f :: Tree Int -> Tree Int
    f t = Tree.Node (2 * Tree.rootLabel t) [t, t]

tree3 :: Tree String
tree3 = fmap show $ treeNat 10

tree4 :: Tree String
tree4 = fmap show $ treePow2 4

