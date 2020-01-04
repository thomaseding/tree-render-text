{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

-- | Configurable text rendering of trees.
module Data.Tree.Render.Text (
  ParentLocation(..),
  ChildOrder(..),
  BranchPath(..),

  renderTreeM,
  RenderOptionsM(..),
  tracedRenderOptionsM,
  tracedRenderOptionsAsciiM,

  renderTree,
  RenderOptions,
  tracedRenderOptions,
  tracedRenderOptionsAscii,
) where

import qualified Control.Monad.State.Strict as M
import qualified Control.Monad.Writer as M
import qualified Data.List as List
import           Data.Monoid ( Endo(Endo, appEndo) )
import qualified Data.Tree as Tree
import           Data.Tree ( Tree )

-- | A difference list on typ 'a'.
type DList a = Endo [a]

-- | Appends a list '[a]' to the output of a 'M.Writer (DList a)'.
tellDList :: [a] -> M.Writer (DList a) ()
tellDList s = M.tell $ Endo (s <>)

-- | Describes where a parent node is rendered, relative to its children.
data ParentLocation
  = ParentBeforeChildren
  | ParentAfterChildren
  | ParentBetweenChildren
  deriving (Show, Eq, Ord)

-- | Describes the render order of a node's children.
data ChildOrder
  = FirstToLast
  | LastToFirst
  deriving (Show, Eq, Ord)

-- | A part of a path along a rendered tree.
data BranchPath
  = BranchUp
  -- ^ Describes a turn going up toward the left.
  --
  -- e.g. @╭─@
  | BranchDown
  -- ^ Describes a turn going down toward the left.
  --
  -- e.g. @╰─@
  | BranchJoin
  -- ^ Describes a T-join of a path going up and down toward the left.
  --
  -- e.g. @├─@
  | BranchContinue
  -- ^ Describes a path going up and down.
  --
  -- e.g. @│ @
  | BranchEmpty
  -- ^ Describes a part that does NOT contain a path piece.
  --
  -- e.g. @  @
  deriving (Show, Eq, Ord)

-- | Options used for rendering a 'Tree label'.
data RenderOptionsM m string label = RenderOptions
  { oParentLocation :: ParentLocation
  -- ^ Controls where parent nodes are rendered.
  , oChildOrder :: ChildOrder
  -- ^ Controls the order a node's children are rendered.
  , oVerticalPad :: Int
  -- ^ The amount of vertical spacing between nodes.
  , oPrependNewLine :: Bool
  -- ^ If 'True', a newline is prepended to the rendered output.
  , oFromString :: String -> string
  -- ^ Promotes a 'String' to a 'string'.
  , oWrite :: string -> m ()
  -- ^ Writes a 'string'.
  , oShowNodeLabel :: label -> string
  -- ^ Shows a 'Tree.rootLabel'.
  , oGetNodeMarker :: label -> string
  -- ^ Get the marker for a node. Although this takes as input a node's 'label',
  -- this should not render the label itself.
  --
  -- The label is passed as an argument to allow things such as:
  --  * Rendering a node marker differently for labels that fail to pass a test.
  --  * Highlighting a node currently being visited.
  --
  -- Simple use cases would use a constant function ignoring the label value.
  , oShowBranchPath :: BranchPath -> string
  -- ^ Shows a 'BranchPath'. The returned values should contain no newlines and
  -- should all be of the same printed width when rendered as text.
  }

-- | An alias of 'RenderOptionsM' for producing pure 'String' renders.
type RenderOptions = RenderOptionsM (M.Writer (DList Char))

-- | Options for producing a line-traced tree using unicode drawing characters.
--
--  This uses:
--      BranchUp       -> "╭─"
--      BranchDown     -> "╰─"
--      BranchJoin     -> "├─"
--      BranchContinue -> "│ "
--      BranchEmpty    -> "  "
--
tracedRenderOptionsM
  :: (String -> string)
  -- ^ Promotes a 'String' to a 'string'.
  -> (string -> m ())
  -- ^ Writes a 'string'.
  -> (label -> string)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptionsM m string label
tracedRenderOptionsM fromString' write' show' = RenderOptions
  { oParentLocation = ParentBeforeChildren
  , oChildOrder = FirstToLast
  , oVerticalPad = 0
  , oPrependNewLine = False
  , oFromString = fromString'
  , oWrite = write'
  , oShowNodeLabel = show'
  , oGetNodeMarker = const $ fromString' "● "
  , oShowBranchPath = fromString' . \case
      BranchUp       -> "╭─"
      BranchDown     -> "╰─"
      BranchJoin     -> "├─"
      BranchContinue -> "│ "
      BranchEmpty    -> "  "
  }

-- | Options for producing a line-traced tree using ASCII characters.
--
--  This uses:
--        BranchUp       -> ",-"
--        BranchDown     -> "`-"
--        BranchJoin     -> "|-"
--        BranchContinue -> "| "
--        BranchEmpty    -> "  "
--
tracedRenderOptionsAsciiM
  :: (String -> string)
  -- ^ Promotes a 'String' to a 'string'.
  -> (string -> m ())
  -- ^ Writes a 'string'.
  -> (label -> string)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptionsM m string label
tracedRenderOptionsAsciiM fromString' write' show' =
  (tracedRenderOptionsM fromString' write' show')
    { oGetNodeMarker = const $ fromString' "o "
    , oShowBranchPath = fromString' . \case
        BranchUp       -> ",-"
        BranchDown     -> "`-"
        BranchJoin     -> "|-"
        BranchContinue -> "| "
        BranchEmpty    -> "  "
    }

-- | Simplified 'tracedRenderOptionsM' when using 'RenderOptions'.
tracedRenderOptions
  :: (label -> String)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptions String label
tracedRenderOptions = tracedRenderOptionsM id tellDList

-- | Simplified 'tracedRenderOptionsAsciiM' when using 'RenderOptions'.
tracedRenderOptionsAscii
  :: (label -> String)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptions String label
tracedRenderOptionsAscii = tracedRenderOptionsAsciiM id tellDList

-- | Renders a 'Tree' a pretty printed tree asa a 'String'.
renderTree :: RenderOptions String label -> Tree label -> String
renderTree options = run . renderTreeM options
  where
    run = ($ "") . appEndo . M.execWriter

-- | Renders a pretty printed tree within a monadic context.
renderTreeM :: Monad m => RenderOptionsM m string label -> Tree label -> m ()
renderTreeM options tree = M.evalStateT action options
  where
    action = render [] tree

type Render string label m = M.StateT (RenderOptionsM m string label) m

write :: Monad m => string -> Render string label m ()
write s = do
  w <- M.gets oWrite
  M.lift $ w s

render :: Monad m => [BranchPath] -> Tree label -> Render string label m ()
render trail = \case
  Tree.Node
    { Tree.rootLabel = label
    , Tree.subForest = kids'
    } -> do

      let renderCurr = do
            getMarker <- M.gets oGetNodeMarker
            showLabel <- M.gets oShowNodeLabel
            M.gets oPrependNewLine >>= \case
              True  -> renderNewLine
              False -> M.modify' $ \st -> st
                { oPrependNewLine = True
                }
            renderTrail trail
            write $ getMarker label
            write $ showLabel label

      childOrder <- M.gets oChildOrder
      let kids = case childOrder of
            FirstToLast -> kids'
            LastToFirst -> reverse kids'

      M.gets oParentLocation >>= \case

        ParentBeforeChildren -> do
          let renderNext path = render $ path : trail
          case initLast kids of
            Nothing -> do
              renderCurr
            Just (ks, k) -> do
              renderCurr
              M.forM_ ks $ \k' -> do
                renderVerticalSpace trail
                renderNext BranchJoin k'
              renderVerticalSpace trail
              renderNext BranchDown k

        ParentAfterChildren -> do
          let renderNext path = render $ path : trail
          case kids of
            [] -> do
              renderCurr
            k : ks -> do
              renderNext BranchUp k
              M.forM_ ks $ \k' -> do
                renderVerticalSpace trail
                renderNext BranchJoin k'
              renderVerticalSpace trail
              renderCurr

        ParentBetweenChildren -> do
          let trailL = case trail of
                BranchDown : rest -> BranchContinue : rest
                _ -> trail
              trailR = case trail of
                BranchUp : rest -> BranchContinue : rest
                _ -> trail
              renderNextL path = render $ path : trailL
              renderNextR path = render $ path : trailR
          case headMiddleLast kids of
            Nothing -> do
              renderCurr
            Just (k, Nothing) -> do
              case childOrder of
                FirstToLast -> do
                  renderCurr
                  renderVerticalSpace trailR
                  renderNextR BranchDown k
                LastToFirst -> do
                  renderNextL BranchUp k
                  renderVerticalSpace trailL
                  renderCurr
            Just (k0, Just (ks, kn)) -> do
              let index = case childOrder of
                    FirstToLast -> length ks `div` 2
                    LastToFirst -> case length ks `divMod` 2 of
                      (d, 0) -> d
                      (d, _) -> d + 1
              let (ksL, ksR) = List.splitAt index ks
              renderNextL BranchUp k0
              M.forM_ ksL $ \k -> do
                renderVerticalSpace trailL
                renderNextL BranchJoin k
              renderVerticalSpace trailL
              renderCurr
              M.forM_ ksR $ \k -> do
                renderVerticalSpace trailR
                renderNextR BranchJoin k
              renderVerticalSpace trailR
              renderNextR BranchDown kn

renderNewLine :: Monad m => Render string label m ()
renderNewLine = do
  from <- M.gets oFromString
  write $ from "\n"

renderVerticalSpace :: Monad m => [BranchPath] -> Render string label m ()
renderVerticalSpace trail = do
  n <- M.gets oVerticalPad
  M.replicateM_ n $ do
    renderNewLine
    renderTrail $ BranchContinue : trail

renderTrail :: Monad m => [BranchPath] -> Render string label m ()
renderTrail trail = do
  showPath <- M.gets oShowBranchPath
  let renderPath = write . showPath
  case trail of
    [] -> pure ()
    p : ps -> do
      M.forM_ (reverse ps) $ renderPath . \case
        BranchDown  -> BranchEmpty
        BranchUp    -> BranchEmpty
        BranchEmpty -> BranchEmpty
        _ -> BranchContinue
      write $ showPath p

initLast :: [a] -> Maybe ([a], a)
initLast = \case
  [] -> Nothing
  xs -> Just (init xs, last xs)

headMiddleLast :: [a] -> Maybe (a, Maybe ([a], a))
headMiddleLast = \case
  [] -> Nothing
  x : xs -> case xs of
    [] -> Just (x, Nothing)
    _  -> Just (x, Just (init xs, last xs))

