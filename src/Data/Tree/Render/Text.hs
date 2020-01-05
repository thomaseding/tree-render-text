{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Safe #-}

-- | Configurable text rendering of trees.
--
-- Example renderings for:
--
-- > import Data.Tree
-- > import Data.Tree.Render.Text
-- >
-- > tree :: Tree String
-- > tree
-- >   = Node "Add"
-- >     [ Node "Add"
-- >       [ Node "0" []
-- >       , Node "Mul"
-- >         [ Node "1" []
-- >         , Node "2" []
-- >         ]
-- >       ]
-- >     , Node "Neg"
-- >       [ Node "Max"
-- >         [ Node "3" []
-- >         , Node "4" []
-- >         , Node "5" []
-- >         , Node "Var"
-- >           [ Node "x" []
-- >           ]
-- >         , Node "6" []
-- >         ]
-- >       ]
-- >     ]
-- >
-- > renderTree (tracedRenderOptions id) tree
-- >
-- > ● Add
-- > ├─● Add
-- > │ ├─● 0
-- > │ ╰─● Mul
-- > │   ├─● 1
-- > │   ╰─● 2
-- > ╰─● Neg
-- >   ╰─● Max
-- >     ├─● 3
-- >     ├─● 4
-- >     ├─● 5
-- >     ├─● Var
-- >     │ ╰─● x
-- >     ╰─● 6
-- >
-- > Other renderings by setting 'ParentLocation' and 'ChildOrder' in the options:
-- >
-- >   ╭─● 0         ╭─● 0       ● Add             ╭─● 6         ╭─● 6
-- >   │ ╭─● 1     ╭─● Add       ├─● Neg           │ ╭─● x       │ ╭─● x
-- >   │ ├─● 2     │ │ ╭─● 1     │ ╰─● Max         ├─● Var       ├─● Var
-- >   ├─● Mul     │ ╰─● Mul     │   ├─● 6         ├─● 5         ├─● 5
-- > ╭─● Add       │   ╰─● 2     │   ├─● Var       ├─● 4       ╭─● Max
-- > │   ╭─● 3     ● Add         │   │ ╰─● x       ├─● 3       │ ├─● 4
-- > │   ├─● 4     ╰─● Neg       │   ├─● 5       ╭─● Max       │ ╰─● 3
-- > │   ├─● 5       │ ╭─● 3     │   ├─● 4     ╭─● Neg       ╭─● Neg
-- > │   │ ╭─● x     │ ├─● 4     │   ╰─● 3     │   ╭─● 2     ● Add
-- > │   ├─● Var     ╰─● Max     ╰─● Add       │   ├─● 1     │   ╭─● 2
-- > │   ├─● 6         ├─● 5       ├─● Mul     │ ╭─● Mul     │ ╭─● Mul
-- > │ ╭─● Max         ├─● Var     │ ├─● 2     │ ├─● 0       │ │ ╰─● 1
-- > ├─● Neg           │ ╰─● x     │ ╰─● 1     ├─● Add       ╰─● Add
-- > ● Add             ╰─● 6       ╰─● 0       ● Add           ╰─● 0
--
module Data.Tree.Render.Text (

  ParentLocation(..),
  ChildOrder(..),
  BranchPath(..),
  LocalContext(..),

  RenderOptionsM(..),
  RenderOptions,

  renderTree,
  renderForest,

  renderTreeM,
  renderForestM,

  tracedRenderOptions,
  tracedRenderOptionsAscii,
  zigZagRenderOptions,
  tabbedRenderOptions,

  tracedRenderOptionsM,
  tracedRenderOptionsAsciiM,
  zigZagRenderOptionsM,
  tabbedRenderOptionsM,

) where

import qualified Control.Monad.State.Strict as M
import qualified Control.Monad.Writer as M
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Data.Monoid ( Endo(Endo, appEndo) )
import qualified Data.Tree as Tree
import           Data.Tree ( Tree, Forest )

-- | A difference list on typ 'a'.
type DList a = Endo [a]

runDListWriter :: M.Writer (DList a) () -> [a]
runDListWriter = ($ []) . appEndo . M.execWriter

-- | Appends a list '[a]' to the output of a 'M.Writer (DList a)'.
tellDList :: [a] -> M.Writer (DList a) ()
tellDList s = M.tell $ Endo (s <>)

-- | Describes where a parent node is rendered, relative to its children.
data ParentLocation
  = ParentBeforeChildren
  -- ^ Renders the before any of its children.
  | ParentAfterChildren
  -- ^ Renders the parent after all of its children.
  | ParentBetweenChildren
  -- ^ Renders the parent in the middle of its children (if there are multiple children).
  -- The index is rounded down when using 'FirstToLast' and rounded up when using 'LastToFirst'.
  | ParentAtChildIndex Int
  -- ^ This is a value from @[0, 1, ..., length children]@ inclusive.
  -- (Values outside this range are clamped to the closest valid value.)
  --
  -- A value of @0@ makes the parent rendered before any of its children
  -- A value of @length children@ makes the parent rendered after all of its children.
  -- Other values place the parent in the corresponding spot between its children.
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
  -- e.g. @"╭─"@
  | BranchDown
  -- ^ Describes a turn going down toward the left.
  --
  -- e.g. @"╰─"@
  | BranchJoin
  -- ^ Describes a T-join of a path going up and down toward the left.
  --
  -- e.g. @"├─"@
  | BranchContinue
  -- ^ Describes a path going up and down.
  --
  -- e.g. @"│ "@
  | BranchEmpty
  -- ^ Describes a part that does NOT contain a path piece.
  --
  -- e.g. @"  "@
  deriving (Show, Eq, Ord)

-- | Local context about a node.
data LocalContext label
  = LocalContext
    { lcCurrentNode :: Tree label
    -- ^ The node assiated with this context.
    , lcCurrentDepth :: !Int
    -- ^ The depth of the current node.
    , lcLitterIndex :: !Int
    -- ^ The index of the current node with respect to its parent's children.
    , lcLitterSize :: !Int
    -- ^ The number of children the current node's parent has.
    }

-- | Options used for rendering a 'Tree'.
data RenderOptionsM m string label = RenderOptions
  { oParentLocation :: Maybe (LocalContext label) -> m ParentLocation
  -- ^ Controls where parent nodes are rendered.
  --
  -- A value of 'Nothing' is passed when rending the artificial root of a 'Forest'.
  --
  -- Simple use cases would use a constant function ignoring the local context.
  , oChildOrder :: Maybe (LocalContext label) -> m ChildOrder
  -- ^ Controls the order a node's children are rendered.
  --
  -- A value of 'Nothing' is passed when rending the artificial root of a 'Forest'.
  --
  -- Simple use cases would use a constant function ignoring the local context.
  , oVerticalPad :: Int
  -- ^ The amount of vertical spacing between nodes.
  , oPrependNewline :: Bool
  -- ^ If 'True', a newline is prepended to the rendered output.
  , oWriteNewline :: m ()
  -- ^ Writes a newline.
  , oWrite :: string -> m ()
  -- ^ Writes a 'string'.
  , oShowNodeLabel :: Maybe label -> m string
  -- ^ Shows a 'Tree.rootLabel'.
  -- The returned value should contain no newlines.
  , oNodeMarker :: Maybe (LocalContext label) -> m string
  -- ^ Get the marker for a node (without rendering its label).
  -- The returned value should contain no newlines.
  --
  -- 'LocalContext' is passed as an argument to allow things such as:
  --
  --    - Rendering a node marker differently for labels that fail to pass a test.
  --    - Highlighting a node currently being visited.
  --    - Numbered bullets.
  --
  -- A value of 'Nothing' is passed when rending the artificial root of a 'Forest'.
  --
  -- Simple use cases would typically ignore the local context.
  , oShowBranchPath :: BranchPath -> string
  -- ^ Shows a 'BranchPath'. The returned value should contain no newlines and
  -- should all be of the same printed width when rendered as text.
  }

mkStringRenderOptionsM
  :: Monad m
  => (Bool -> String)
  -> (BranchPath -> String)
  -> (String -> string)
  -> (string -> m ())
  -> (label -> m string)
  -> RenderOptionsM m string label
mkStringRenderOptionsM showMarker showPath fromStr write showLabel
  = RenderOptions
    { oParentLocation = const loc
    , oChildOrder = const ord
    , oVerticalPad = 0
    , oPrependNewline = False
    , oWriteNewline = write newline
    , oWrite = write
    , oShowNodeLabel = maybe nil showLabel
    , oNodeMarker = \case
        Just {} -> node
        Nothing -> root
    , oShowBranchPath = \case
        BranchUp       -> up
        BranchDown     -> down
        BranchJoin     -> join
        BranchContinue -> continue
        BranchEmpty    -> empty
    }
  where
    loc = pure ParentBeforeChildren
    ord = pure FirstToLast
    nil  = pure $ fromStr ""
    node = pure $ fromStr $ showMarker True
    root = pure $ fromStr $ showMarker False
    up       = fromStr $ showPath BranchUp
    down     = fromStr $ showPath BranchDown
    join     = fromStr $ showPath BranchJoin
    continue = fromStr $ showPath BranchContinue
    empty    = fromStr $ showPath BranchEmpty
    newline  = fromStr "\n"

unicodeMarker :: Bool -> String
unicodeMarker = \case
  True  -> "● "
  False -> "●"

unicodePath :: BranchPath -> String
unicodePath = \case
  BranchUp       -> "╭─"
  BranchDown     -> "╰─"
  BranchJoin     -> "├─"
  BranchContinue -> "│ "
  BranchEmpty    -> "  "

-- | Options for rendering a line-traced tree using unicode drawing characters.
--
--  This uses:
--
-- > BranchUp       -> "╭─"
-- > BranchDown     -> "╰─"
-- > BranchJoin     -> "├─"
-- > BranchContinue -> "│ "
-- > BranchEmpty    -> "  "
--
-- > oNodeMarker = \case
-- >   Just {} -> "● "
-- >   Nothing -> "●"
--
tracedRenderOptionsM
  :: Monad m
  => (String -> string)
  -- ^ Promotes a 'String' to a 'string'.
  -> (string -> m ())
  -- ^ Writes a 'string'.
  -> (label -> m string)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptionsM m string label
tracedRenderOptionsM = mkStringRenderOptionsM unicodeMarker unicodePath

-- | Options for rendering a line-traced tree using ASCII characters.
--
--  This uses:
--
-- > BranchUp       -> ",-"
-- > BranchDown     -> "`-"
-- > BranchJoin     -> "|-"
-- > BranchContinue -> "| "
-- > BranchEmpty    -> "  "
--
-- > oNodeMarker = \case
-- >   Just {} -> "o "
-- >   Nothing -> "o"
--
tracedRenderOptionsAsciiM
  :: Monad m
  => (String -> string)
  -- ^ Promotes a 'String' to a 'string'.
  -> (string -> m ())
  -- ^ Writes a 'string'.
  -> (label -> m string)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptionsM m string label
tracedRenderOptionsAsciiM = mkStringRenderOptionsM marker path
  where
    marker = \case
      True  -> "o "
      False -> "o"
    path = \case
      BranchUp       -> ",-"
      BranchDown     -> "`-"
      BranchJoin     -> "|-"
      BranchContinue -> "| "
      BranchEmpty    -> "  "

-- | A variety on 'tracedRenderOptionsM' where the path tracing is
-- performed in a zig-zag fashion.
zigZagRenderOptionsM
  :: Monad m
  => (String -> string)
  -- ^ Promotes a 'String' to a 'string'.
  -> (string -> m ())
  -- ^ Writes a 'string'.
  -> (label -> m string)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptionsM m string label
zigZagRenderOptionsM fromStr write showLabel = options
  { oParentLocation = pure . \case
      Nothing -> ParentBeforeChildren
      Just LocalContext
        { lcLitterIndex = index
        , lcLitterSize  = size
        } -> case index < (size `div` 2) of
          True  -> ParentBeforeChildren
          False -> ParentAfterChildren
  }
  where
    options = tracedRenderOptionsM fromStr write showLabel

-- | Options for rendering a tree in rows indented only by tabs.
tabbedRenderOptionsM
  :: Monad m
  => String
  -- ^ The string used for a tab.
  -> (String -> string)
  -- ^ Promotes a 'String' to a 'string'.
  -> (string -> m ())
  -- ^ Writes a 'string'.
  -> (label -> m string)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptionsM m string label
tabbedRenderOptionsM tab = mkStringRenderOptionsM marker path
  where
    marker = const ""
    path = const tab

-- | An alias of 'RenderOptionsM' for producing pure 'String' renders.
type RenderOptions = RenderOptionsM (M.Writer (DList Char))

-- | A simplified 'tracedRenderOptionsM' specialized to @RenderOptions@.
tracedRenderOptions
  :: (label -> String)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptions String label
tracedRenderOptions = tracedRenderOptionsM id tellDList . fmap pure

-- | A simplified 'tabbedRenderOptionsM' specialized to @RenderOptions@.
tabbedRenderOptions
  :: String
  -- ^ The string used for a tab.
  -> (label -> String)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptions String label
tabbedRenderOptions tab = tabbedRenderOptionsM tab id tellDList . fmap pure

-- | A simplified 'zigZagRenderOptionsM' specialized to @RenderOptions@.
zigZagRenderOptions
  :: (label -> String)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptions String label
zigZagRenderOptions = zigZagRenderOptionsM id tellDList . fmap pure

-- | A simplified 'tracedRenderOptionsAsciiM' specialized to @RenderOptions@.
tracedRenderOptionsAscii
  :: (label -> String)
  -- ^ Shows a 'Tree.rootLabel'.
  -> RenderOptions String label
tracedRenderOptionsAscii = tracedRenderOptionsAsciiM id tellDList . fmap pure

-- | Renders a 'Tree' to a 'String'.
renderTree :: RenderOptions String label -> Tree label -> String
renderTree options = runDListWriter . renderTreeM options

-- | Renders a 'Forest' to a 'String'.
renderForest :: RenderOptions String label -> Forest label -> String
renderForest options = runDListWriter . renderForestM options

-- | Renders a pretty printed 'Tree' within a monadic context.
renderTreeM :: Monad m => RenderOptionsM m string label -> Tree label -> m ()
renderTreeM options tree = M.evalStateT action options
  where
    action = render lc []
    lc = LocalContext
      { lcCurrentNode = tree
      , lcCurrentDepth = 0
      , lcLitterIndex = 0
      , lcLitterSize = 1
      }

catMaybes :: Tree (Maybe label) -> Maybe (Tree label)
catMaybes = \case
  Tree.Node
    { Tree.rootLabel = mLabel
    , Tree.subForest = kids
    } -> case mLabel of
      Nothing -> Nothing
      Just label -> Just Tree.Node
        { Tree.rootLabel = label
        , Tree.subForest = Maybe.mapMaybe catMaybes kids
        }

-- | Renders a pretty printed 'Forest' within a monadic context.
renderForestM :: Monad m => RenderOptionsM m string label -> Forest label -> m ()
renderForestM options trees = do
  let forestTree = Tree.Node Nothing $ map (fmap Just) trees
  let flattenLc = \case
            Nothing -> Nothing
            Just lc ->
              let node = lcCurrentNode lc
              in case catMaybes node of
                Nothing -> Nothing
                Just node' -> Just lc { lcCurrentNode = node' }
  let options' = options
        { oShowNodeLabel = oShowNodeLabel options . maybe Nothing id
        , oParentLocation = oParentLocation options . flattenLc
        , oChildOrder     = oChildOrder     options . flattenLc
        , oNodeMarker     = oNodeMarker     options . flattenLc
        }
  renderTreeM options' forestTree

type Render string label m = M.StateT (RenderOptionsM m string label) m

renderString :: Monad m => string -> Render string label m ()
renderString s = do
  w <- M.gets oWrite
  M.lift $ w s

writeNewline :: Monad m => Render string label m ()
writeNewline = M.gets oWriteNewline >>= M.lift

render :: Monad m => LocalContext label -> [BranchPath] -> Render string label m ()
render lc trail = case lcCurrentNode lc of
  Tree.Node
    { Tree.rootLabel = label
    , Tree.subForest = kids'
    } -> do

      parentLoc <- M.gets (flip oParentLocation $ Just lc) >>= M.lift
      childOrder <- M.gets (flip oChildOrder $ Just lc) >>= M.lift

      let renderCurr = do
            M.gets oPrependNewline >>= \case
              True  -> writeNewline
              False -> M.modify' $ \st -> st
                { oPrependNewline = True
                }
            renderTrail trail
            marker <- M.gets (flip oNodeMarker $ Just lc) >>= M.lift
            renderString marker
            shownLabel <- M.gets (flip oShowNodeLabel $ Just label) >>= M.lift
            renderString shownLabel

      let kidCount = length kids'
      let kids =
            let f = case childOrder of
                  FirstToLast -> id
                  LastToFirst -> reverse
            in flip map (f $ zip kids' [0..]) $ \(kid, idx) -> LocalContext
              { lcCurrentNode = kid
              , lcCurrentDepth = lcCurrentDepth lc + 1
              , lcLitterIndex = idx
              , lcLitterSize = kidCount
              }

      let trailL = case trail of
            BranchDown : rest -> BranchContinue : rest
            _ -> trail
          trailR = case trail of
            BranchUp : rest -> BranchContinue : rest
            _ -> trail
          renderNextL path lc' = render lc' (path : trailL)
          renderNextR path lc' = render lc' (path : trailR)

      let index = case parentLoc of
            ParentBeforeChildren -> 0
            ParentAfterChildren -> kidCount
            ParentBetweenChildren -> case childOrder of
              FirstToLast -> kidCount `div` 2
              LastToFirst -> case kidCount `divMod` 2 of
                (d, 0) -> d
                (d, _) -> d + 1
            ParentAtChildIndex i -> max 0 $ min kidCount i

      case (index == 0, index == kidCount) of

        (True, _ ) -> do
          case initLast kids of
            Nothing -> do
              renderCurr
            Just (ks, k) -> do
              renderCurr
              M.forM_ ks $ \k' -> do
                renderVerticalSpace trailR
                renderNextR BranchJoin k'
              renderVerticalSpace trailR
              renderNextR BranchDown k

        ( _, True) -> do
          case kids of
            [] -> do
              renderCurr
            k : ks -> do
              renderNextL BranchUp k
              M.forM_ ks $ \k' -> do
                renderVerticalSpace trailL
                renderNextL BranchJoin k'
              renderVerticalSpace trailL
              renderCurr

        ( _ , _ ) -> do
          case headMiddleLast kids of
            Nothing -> undefined -- This can't happen.
            Just (_, Nothing) -> undefined -- This can't happen.
            Just (k0, Just (ks, kn)) -> do
              let (ksL, ksR) = List.splitAt (index - 1) ks
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

renderVerticalSpace :: Monad m => [BranchPath] -> Render string label m ()
renderVerticalSpace trail = do
  n <- M.gets oVerticalPad
  M.replicateM_ n $ do
    writeNewline
    renderTrail $ BranchContinue : trail

renderTrail :: Monad m => [BranchPath] -> Render string label m ()
renderTrail trail = do
  showPath <- M.gets oShowBranchPath
  let renderPath = renderString . showPath
  case trail of
    [] -> pure ()
    p : ps -> do
      M.forM_ (reverse ps) $ renderPath . \case
        BranchDown  -> BranchEmpty
        BranchUp    -> BranchEmpty
        BranchEmpty -> BranchEmpty
        _ -> BranchContinue
      renderString $ showPath p

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

