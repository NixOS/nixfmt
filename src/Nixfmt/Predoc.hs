{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Predoc (
  text,
  comment,
  trailingComment,
  trailing,
  sepBy,
  surroundWith,
  hcat,
  group,
  group',
  nest,
  offset,
  softline',
  line',
  softline,
  line,
  hardspace,
  hardline,
  emptyline,
  newline,
  DocE,
  Doc,
  GroupAnn (..),
  Pretty,
  pretty,
  fixup,
  unexpandSpacing',
  layout,
  textWidth,
)
where

import Control.Applicative (asum, empty, (<|>))
import Control.Monad.Trans.State.Strict (State, StateT (..), evalState, get, mapStateT, modify, put, runState, state)
import Data.Bifunctor (first, second)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.Functor.Identity (runIdentity)
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..), singleton, (<|))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, concat, length, replicate, strip)
import GHC.Stack (HasCallStack)
import Nixfmt.Types (
  LanguageElement,
  mapAllTokens,
  removeLineInfo,
 )

-- | Sequential Spacings are reduced to a single Spacing by taking the maximum.
-- This means that e.g. a Space followed by an Emptyline results in just an
-- Emptyline.
data Spacing
  = -- | Line break or nothing (soft)
    Softbreak
  | -- | Line break or nothing
    Break
  | -- | Always a space
    Hardspace
  | -- | Line break or space (soft)
    Softspace
  | -- | Line break or space
    Space
  | -- | Always a line break
    Hardline
  | -- | Two line breaks
    Emptyline
  | -- | n line breaks
    Newlines Int
  deriving (Show, Eq, Ord)

-- | `Group docs` indicates that either all or none of the Spaces and Breaks
-- in `docs` should be converted to line breaks. This does not affect softlines,
-- those will be expanded only as necessary and with a lower priority.
data GroupAnn
  = RegularG
  | -- Group with priority expansion. This is only rarely needed, and mostly useful
    -- to compact things left and right of a multiline element as long as they fit onto one line.
    --
    -- Groups containing priority groups are treated as having three segments:
    -- pre, prio and post.
    -- If any group contains a priority group, the following happens:
    -- If it entirely fits on one line, render on one line (as usual).
    -- If it does not fit on one line, but pre and post do when prio is expanded, then try that.
    -- In all other cases, fully expand the group as if it didn't contain any priority groups.
    --
    -- If a group contains multiple priority groups, then the renderer will attempt to expand them,
    -- each one individually, and in *reverse* order. If all of these fail, then the entire group
    -- will be fully expanded as if it didn't contain any priority groups.
    Priority
  | -- Usually, priority groups are associated and handled by their direct parent group. However,
    -- if the parent is a `Transparent` group, then they will be associated with its parent instead.
    -- (This goes on transitively until the first non-transparent parent group.)
    -- In the case of priority group expansion, this group will be treated as non-existent (transparent).
    -- Otherwise, it will be treated like a regular group.
    Transparent
  deriving (Show, Eq)

-- Comments do not count towards some line length limits
-- Trailing tokens have the property that they will only exist in expanded groups, and "swallowed" in compact groups
-- Trailing comments are like comments, but marked differently for special treatment further down the line
-- (The difference is that trailing comments are guaranteed to be single "# text" tokens, while all other comments
-- may be composite and multi-line)
data TextAnn = RegularT | Comment | TrailingComment | Trailing
  deriving (Show, Eq)

-- | Single document element. Documents are modeled as lists of these elements
-- in order to make concatenation simple.
data DocE
  = -- nesting depth, offset, kind, text
    Text Int Int TextAnn Text
  | Spacing Spacing
  | Group GroupAnn Doc
  deriving (Show, Eq)

type Doc = [DocE]

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc where
  pretty = id

instance (Pretty a) => Pretty (Maybe a) where
  pretty Nothing = mempty
  pretty (Just x) = pretty x

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = pretty a <> pretty b

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (a, b, c) = pretty a <> pretty b <> pretty c

text :: Text -> Doc
text "" = []
text t = [Text 0 0 RegularT t]

comment :: Text -> Doc
comment "" = []
comment t = [Text 0 0 Comment t]

-- Comment at the end of a line
trailingComment :: Text -> Doc
trailingComment "" = []
trailingComment t = [Text 0 0 TrailingComment t]

-- Text tokens that are only needed in expanded groups
trailing :: Text -> Doc
trailing "" = []
trailing t = [Text 0 0 Trailing t]

-- | Group document elements together (see Node Group documentation)
-- Must not contain non-hard whitespace (e.g. line, softline' etc.) at the start of the end.
-- Use group' for that instead if you are sure of what you are doing.
group :: (HasCallStack) => (Pretty a) => a -> Doc
group x =
  pure . Group RegularG $
    if p /= [] && (isSoftSpacing (head p) || isSoftSpacing (last p))
      then error $ "group should not start or end with whitespace, use `group'` if you are sure; " <> show p
      else p
  where
    p = pretty x

-- | Group document elements together (see Node Group documentation)
-- Is allowed to start or end with any kind of whitespace.
-- Use with caution, and only in situations where you control the surroundings of
-- that group. Especially, never use as a top-level element of a `pretty` instance,
-- or you'll get some *very* confusing bugs …
--
-- Also allows to create priority groups (see Node Group documentation)
group' :: (Pretty a) => GroupAnn -> a -> Doc
group' ann = pure . Group ann . pretty

-- | @nest doc@ declarse @doc@ to have a higher nesting depth
-- than before. Not all nestings actually result in indentation changes,
-- this will be calculated automatically later on. As a rule of thumb:
-- Multiple nesting levels on one line will be compacted and only result in a single
-- indentation bump for the next line. This prevents excessive indentation.
nest :: (Pretty a) => a -> Doc
nest x = map go $ pretty x
  where
    go (Text i o ann t) = Text (i + 1) o ann t
    go (Group ann inner) = Group ann (map go inner)
    go spacing = spacing

-- This is similar to nest, however it circumvents the "smart" rules that usually apply.
-- This should only be useful to manage the indentation within indented strings.
offset :: (Pretty a) => Int -> a -> Doc
offset level x = map go $ pretty x
  where
    go (Text i o ann t) = Text i (o + level) ann t
    go (Group ann inner) = Group ann (map go inner)
    go spacing = spacing

-- | Line break or nothing (soft)
softline' :: Doc
softline' = [Spacing Softbreak]

-- | Line break or nothing
line' :: Doc
line' = [Spacing Break]

-- | Line break or space (soft)
softline :: Doc
softline = [Spacing Softspace]

-- | Line break or space
line :: Doc
line = [Spacing Space]

-- | Always space
hardspace :: Doc
hardspace = [Spacing Hardspace]

-- | Always line break
hardline :: Doc
hardline = [Spacing Hardline]

-- | Two line breaks
emptyline :: Doc
emptyline = [Spacing Emptyline]

newline :: Doc
newline = [Spacing (Newlines 1)]

surroundWith :: (Pretty a) => Doc -> a -> Doc
surroundWith outside inner = outside <> pretty inner <> outside

sepBy :: (Pretty a) => Doc -> [a] -> Doc
sepBy separator = mconcat . intersperse separator . map pretty

-- | Concatenate documents horizontally without spacing.
hcat :: (Pretty a) => [a] -> Doc
hcat = mconcat . map pretty

-- Everything that may change representation depending on grouping
isSoftSpacing :: DocE -> Bool
isSoftSpacing (Spacing Softbreak) = True
isSoftSpacing (Spacing Break) = True
isSoftSpacing (Spacing Softspace) = True
isSoftSpacing (Spacing Space) = True
isSoftSpacing _ = False

-- Everything else
isHardSpacing :: DocE -> Bool
isHardSpacing (Spacing Hardspace) = True
isHardSpacing (Spacing Hardline) = True
isHardSpacing (Spacing Emptyline) = True
isHardSpacing (Spacing (Newlines _)) = True
isHardSpacing _ = False

-- Check if an element is a comment
-- Some comments are nested as nodes with multiple elements.
-- Therefore nodes are counted as comments if they only contain comments or hard spacings
isComment :: DocE -> Bool
isComment (Text _ _ Comment _) = True
isComment (Text _ _ TrailingComment _) = True
isComment (Group _ inner) = all (\x -> isComment x || isHardSpacing x) inner
isComment _ = False

--- Manually force a group to its compact layout, by replacing all relevant whitespace.
--- Does not recurse into inner groups.
unexpandSpacing :: Doc -> Doc
unexpandSpacing [] = []
unexpandSpacing ((Spacing Space) : xs) = Spacing Hardspace : unexpandSpacing xs
unexpandSpacing ((Spacing Softspace) : xs) = Spacing Hardspace : unexpandSpacing xs
unexpandSpacing ((Spacing Break) : xs) = unexpandSpacing xs
unexpandSpacing ((Spacing Softbreak) : xs) = unexpandSpacing xs
unexpandSpacing (s@(Spacing _) : xs) = s : unexpandSpacing xs
unexpandSpacing (x : xs) = x : unexpandSpacing xs

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = fmap reverse . span p . reverse

-- Manually force a group to its compact layout, by replacing all relevant whitespace.
-- Does recurse into inner groups.
-- An optional maximum line length limit may be specified.
-- Fails if the doc contains hardlines or exceeds the length limit
unexpandSpacing' :: Maybe Int -> Doc -> Maybe Doc
unexpandSpacing' (Just n) _ | n < 0 = Nothing
unexpandSpacing' _ [] = Just []
unexpandSpacing' n (txt@(Text _ _ _ t) : xs) = (txt :) <$> unexpandSpacing' (n <&> subtract (textWidth t)) xs
unexpandSpacing' n (Spacing Hardspace : xs) = (Spacing Hardspace :) <$> unexpandSpacing' (n <&> subtract 1) xs
unexpandSpacing' n (Spacing Space : xs) = (Spacing Hardspace :) <$> unexpandSpacing' (n <&> subtract 1) xs
unexpandSpacing' n (Spacing Softspace : xs) = (Spacing Hardspace :) <$> unexpandSpacing' (n <&> subtract 1) xs
unexpandSpacing' n (Spacing Break : xs) = unexpandSpacing' n xs
unexpandSpacing' n (Spacing Softbreak : xs) = unexpandSpacing' n xs
unexpandSpacing' _ (Spacing _ : _) = Nothing
unexpandSpacing' n ((Group _ xs) : ys) = unexpandSpacing' n $ xs <> ys

-- Dissolve some groups with only one item
simplifyGroup :: GroupAnn -> Doc -> Doc
simplifyGroup _ [] = []
simplifyGroup ann [Group ann' body] | ann == ann' = body
simplifyGroup _ x = x

-- | Fix up a Doc:
-- - Move some spacings (those which are not relevant for group calculations)
--   out of the start/end of Groups and Nests if possible.
--   This is especially important because it moves out hardlines from comments out of groups,
--   which would otherwise wrongly cause them to expand.
-- - Merge consecutive spacings.
-- - Spacings are not merged across Group or Nest boundaries, although this may happen for those
--   spacings that are moved.
-- - Remove empty Groups and Nests
-- After running, any nodes are guaranteed to start/end with at most one whitespace element respectively.
fixup :: Doc -> Doc
fixup [] = []
-- Merge consecutive spacings
fixup (Spacing a : Spacing b : xs) = fixup $ Spacing (mergeSpacings a b) : xs
-- Merge consecutive texts. Take indentation and offset from the left one
fixup (Text level off ann a : Text _ _ ann' b : xs) | ann == ann' = fixup $ Text level off ann (a <> b) : xs
-- Move/Merge hard spaces into groups
fixup ((Spacing Hardspace) : Group ann xs : ys) = fixup $ Group ann (Spacing Hardspace : xs) : ys
-- Handle group, with stuff in front of it to potentially merge with
fixup (a@(Spacing _) : Group ann xs : ys) =
  let -- Recurse onto xs, split out leading and trailing whitespace into pre and post.
      -- For the leading side, also move out comments out of groups, they are kinda the same thing
      -- (We could move out trailing comments too but it would make no difference)
      (pre, rest) = span (\x -> isHardSpacing x || isComment x) $ fixup xs
      (post, body) = second (simplifyGroup ann) $ spanEnd isHardSpacing rest
  in if null body
      then -- Dissolve empty group
        fixup $ (a : pre) ++ post ++ ys
      else fixup (a : pre) ++ [Group ann body] ++ fixup (post ++ ys)
-- Handle group, almost the same thing as above
fixup (Group ann xs : ys) =
  let (pre, rest) = span (\x -> isHardSpacing x || isComment x) $ fixup xs
      (post, body) = second (simplifyGroup ann) $ spanEnd isHardSpacing rest
  in if null body
      then fixup $ pre ++ post ++ ys
      else fixup pre ++ [Group ann body] ++ fixup (post ++ ys)
fixup (x : xs) = x : fixup xs

mergeSpacings :: Spacing -> Spacing -> Spacing
mergeSpacings x y | x > y = mergeSpacings y x
mergeSpacings Break Softspace = Space
mergeSpacings Break Hardspace = Space
mergeSpacings Softbreak Hardspace = Softspace
mergeSpacings (Newlines x) (Newlines y) = Newlines (x + y)
mergeSpacings Emptyline (Newlines x) = Newlines (x + 2)
mergeSpacings Hardspace (Newlines x) = Newlines x
mergeSpacings _ (Newlines x) = Newlines (x + 1)
mergeSpacings _ y = y

layout :: (Pretty a, LanguageElement a) => Int -> Bool -> a -> Text
layout width strict =
  (<> "\n")
    . Text.strip
    . layoutGreedy width
    . fixup
    . pretty
    -- In strict mode, set the line number of all tokens to zero
    . (if strict then mapAllTokens removeLineInfo else id)

-- 1. Move and merge Spacings.
-- 2. Convert Softlines to Grouped Lines and Hardspaces to Texts.
-- 3. For each Text or Group, try to fit as much as possible on a line
-- 4. For each Group, if it fits on a single line, render it that way.
-- 5. If not, convert lines to hardlines and unwrap the group

-- Extract and list the priority groups of this group.
-- The return value is a segmentation of the input, each segment annotated with its priority (True = Priority).
-- This recurses into `Transparent` subgroups on the search for priority groups, and flattens their content in the output.
-- If no priority groups are found, the empty list is returned.
priorityGroups :: Doc -> [(Doc, Doc, Doc)]
priorityGroups = explode . mergeSegments . segments
  where
    segments :: Doc -> [(Bool, Doc)]
    segments [] = []
    segments ((Group Priority ys) : xs) = (True, ys) : segments xs
    segments ((Group Transparent ys) : xs) = segments ys ++ segments xs
    segments (x : xs) = (False, pure x) : segments xs

    -- Merge subsequent segments of non-priority-group elements
    mergeSegments :: [(Bool, Doc)] -> [(Bool, Doc)]
    mergeSegments [] = []
    mergeSegments ((False, content1) : (False, content2) : xs) = mergeSegments $ (False, content1 ++ content2) : xs
    mergeSegments (x : xs) = x : mergeSegments xs

    -- Convert the segmented/pre-porcessed input into a list of all groups as (pre, prio, post) triples
    explode :: [(Bool, Doc)] -> [(Doc, Doc, Doc)]
    explode [] = []
    explode [(prio, x)]
      | prio = [([], x, [])]
      | otherwise = []
    explode ((prio, x) : xs)
      | prio = ([], x, concatMap snd xs) : map (\(a, b, c) -> (x <> a, b, c)) (explode xs)
      | otherwise = map (\(a, b, c) -> (x <> a, b, c)) (explode xs)

-- | To support i18n, this function needs to be patched.
textWidth :: Text -> Int
textWidth = Text.length

-- | Attempt to fit a list of documents in a single line of a specific width.
-- ni — next indentation. Only used for trailing comment calculations. Set this to the indentation
--      of the next line relative to the current one. So usuall 2 when the indentation level increases, 0 otherwise.
-- c — allowed width
fits :: Int -> Int -> Doc -> Maybe Text
fits _ c _ | c < 0 = Nothing
fits _ _ [] = Just ""
-- This case is impossible in the input thanks to fixup, but may happen
-- due to our recursion on nodes below
fits ni c (Spacing a : Spacing b : xs) = fits ni c (Spacing (mergeSpacings a b) : xs)
fits ni c (x : xs) = case x of
  Text _ _ RegularT t -> (t <>) <$> fits (ni - textWidth t) (c - textWidth t) xs
  Text _ _ Comment t -> (t <>) <$> fits ni c xs
  Text _ _ TrailingComment t
    | ni == 0 -> ((" " <> t) <>) <$> fits ni c xs
    | otherwise -> (t <>) <$> fits ni c xs
  Text _ _ Trailing _ -> fits ni c xs
  Spacing Softbreak -> fits ni c xs
  Spacing Break -> fits ni c xs
  Spacing Softspace -> (" " <>) <$> fits (ni - 1) (c - 1) xs
  Spacing Space -> (" " <>) <$> fits (ni - 1) (c - 1) xs
  Spacing Hardspace -> (" " <>) <$> fits (ni - 1) (c - 1) xs
  Spacing Hardline -> Nothing
  Spacing Emptyline -> Nothing
  Spacing (Newlines _) -> Nothing
  Group _ ys -> fits ni c $ ys ++ xs

-- | Find the width of the first line in a list of documents, using target
-- width 0, which always forces line breaks when possible.
firstLineWidth :: Doc -> Int
firstLineWidth [] = 0
firstLineWidth (Text _ _ Comment _ : xs) = firstLineWidth xs
firstLineWidth (Text _ _ TrailingComment _ : xs) = firstLineWidth xs
firstLineWidth (Text _ _ _ t : xs) = textWidth t + firstLineWidth xs
-- This case is impossible in the input thanks to fixup, but may happen
-- due to our recursion on groups below
firstLineWidth (Spacing a : Spacing b : xs) = firstLineWidth (Spacing (mergeSpacings a b) : xs)
firstLineWidth (Spacing Hardspace : xs) = 1 + firstLineWidth xs
firstLineWidth (Spacing _ : _) = 0
firstLineWidth (Group _ xs : ys) = firstLineWidth $ xs ++ ys

-- | Check if the first line in a document fits a target width given
-- a maximum width, without breaking up groups.
firstLineFits :: Int -> Int -> Doc -> Bool
firstLineFits targetWidth maxWidth docs = go maxWidth docs
  where
    go c _ | c < 0 = False
    go c [] = maxWidth - c <= targetWidth
    go c (Text _ _ RegularT t : xs) = go (c - textWidth t) xs
    go c (Text{} : xs) = go c xs
    -- This case is impossible in the input thanks to fixup, but may happen
    -- due to our recursion on groups below
    go c (Spacing a : Spacing b : xs) = go c $ Spacing (mergeSpacings a b) : xs
    go c (Spacing Hardspace : xs) = go (c - 1) xs
    go c (Spacing _ : _) = maxWidth - c <= targetWidth
    go c (Group _ ys : xs) =
      case fits 0 (c - firstLineWidth xs) ys of
        Nothing -> go c (ys ++ xs)
        Just t -> go (c - textWidth t) xs

-- Calculate the amount of indentation until the first token
-- This assumes the input to be an unexpanded group at the start of a new line
nextIndent :: Doc -> (Int, Int)
nextIndent ((Text i o _ _) : _) = (i, o)
nextIndent ((Group _ xs) : _) = nextIndent xs
nextIndent (_ : xs) = nextIndent xs
nextIndent _ = (0, 0)

-- | Create `n` newlines
newlines :: Int -> Text
newlines n = Text.replicate n "\n"

-- | Create `n` spaces
indent :: Int -> Text
indent n = Text.replicate n " "

-- All state is (cc, indents)
-- cc: current column (within the current line, *not including indentation*)
-- indents:
--   A stack of tuples (currentIndent, nestingLevel)
--   This is guaranteed to never be empty, as we start with [(0, 0)] and never go below that.
type St = (Int, NonEmpty (Int, Int))

-- tw   Target Width
layoutGreedy :: Int -> Doc -> Text
layoutGreedy tw doc = Text.concat $ evalState (go [Group RegularG doc] []) (0, singleton (0, 0))
  where
    -- Simple helpers around `put` with a tuple state
    putL = modify . first . const
    putR = modify . second . const

    -- Print a given text. If this is the first token on a line, it will
    -- do the appropriate calculations for indentation and print that in addition to the text.
    putText :: Int -> Int -> Text -> State St [Text]
    putText textNL textOffset t =
      get
        >>= \(cc, indents@((ci, nl) :| indents')) ->
          case textNL `compare` nl of
            -- Push the textNL onto the stack, but only increase the actual indentation (`ci`)
            -- if this is the first one of a line. All subsequent nestings within the line effectively get "swallowed"
            GT -> putR ((if cc == 0 then ci + 2 else ci, textNL) <| indents) >> go'
            -- Need to go down one or more levels
            -- Just pop from the stack and recurse until the indent matches again
            LT -> putR (NonEmpty.fromList indents') >> putText textNL textOffset t
            EQ -> go'
      where
        -- Put the text and advance the `cc` cursor. Add the appropriate amount of indentation if this is
        -- the first token on a line
        go' = do
          (cc, (ci, _) :| _) <- get
          putL (cc + textWidth t)
          pure $ if cc == 0 then [indent (ci + textOffset), t] else [t]

    -- Simply put text without caring about line-start indentation
    putText' :: [Text] -> State St [Text]
    putText' ts = do
      (cc, indents) <- get
      put (cc + sum (map textWidth ts), indents)
      pure ts

    -- First argument: chunks to render
    -- Second argument: lookahead of following chunks, not rendered
    go :: Doc -> Doc -> State St [Text]
    go [] _ = return []
    go (x : xs) ys = do t <- goOne x (xs ++ ys); ts <- go xs ys; return (t ++ ts)

    -- First argument: chunk to render. This will recurse into nests/groups if the chunk is one.
    -- Second argument: lookahead of following chunks
    goOne :: DocE -> Doc -> State St [Text]
    goOne x xs =
      get >>= \(cc, indents) ->
        let -- The last printed character was a line break
            needsIndent = (cc == 0)

            putNL :: Int -> State St [Text]
            putNL n = put (0, indents) $> [newlines n]
        in case x of
            -- Special case trailing comments. Because in cases like
            -- [ # comment
            --   1
            -- ]
            -- the comment will be parsed as associated to the inner element next time, rendering it as
            -- [
            --   # comment
            --   1
            -- ]
            -- This breaks idempotency. To work around this, we simply shift the comment by one:
            -- [  # comment
            --   1
            -- ]
            Text _ _ TrailingComment t | cc == 2 && fst (nextIndent xs) > lineNL -> putText' [" ", t]
              where
                lineNL = snd $ NonEmpty.head indents
            Text nl off _ t -> putText nl off t
            -- This code treats whitespace as "expanded"
            -- A new line resets the column counter and sets the target indentation as current indentation
            Spacing sp
              -- We know that the last printed character was a line break (cc == 0),
              -- therefore drop any leading whitespace within the group to avoid duplicate newlines
              | needsIndent -> pure []
              | otherwise -> case sp of
                  Break -> putNL 1
                  Space -> putNL 1
                  Hardspace -> putText' [" "]
                  Hardline -> putNL 1
                  Emptyline -> putNL 2
                  (Newlines n) -> putNL n
                  Softbreak
                    | firstLineFits (tw - cc) tw xs ->
                        pure []
                    | otherwise -> putNL 1
                  Softspace
                    | firstLineFits (tw - cc - 1) tw xs ->
                        putText' [" "]
                    | otherwise -> putNL 1
            Group ann ys ->
              let -- fromMaybe lifted to (StateT s Maybe)
                  fromMaybeState :: State s a -> StateT s Maybe a -> State s a
                  fromMaybeState l r = state $ \s -> fromMaybe (runState l s) (runStateT r s)
              in -- Try to fit the entire group first
                 goGroup ys xs
                  -- If that fails, check whether the group contains any priority groups within its children and try to expand them first
                  -- Ignore transparent groups as their priority children have already been handled up in the parent (and failed)
                  <|> ( if ann /= Transparent
                          then -- Each priority group will be handled individually, and the priority groups are tried in reverse order
                            asum $ map (`goPriorityGroup` xs) $ reverse $ priorityGroups ys
                          else empty
                      )
                  -- Otherwise, dissolve the group by mapping its members to the target indentation
                  -- This also implies that whitespace in there will now be rendered "expanded".
                  & fromMaybeState (go ys xs)

    goPriorityGroup :: (Doc, Doc, Doc) -> Doc -> StateT St Maybe [Text]
    goPriorityGroup (pre, prio, post) rest = do
      -- Try to fit pre onto one line
      preRendered <- goGroup pre (prio ++ post ++ rest)
      -- Render prio expanded
      -- We know that post will be rendered compact. So we tell the renderer that by manually removing all
      -- line breaks in post here. Otherwise we might get into awkward the situation where pre and prio are put
      -- onto the one line, all three obviously wouldn't fit.
      prioRendered <-
        mapStateT (Just . runIdentity) $
          go prio (unexpandSpacing post ++ rest)
      -- Try to render post onto one line
      postRendered <- goGroup post rest
      -- If none of these failed, put together and return
      return (preRendered ++ prioRendered ++ postRendered)

    -- Try to fit the group onto a single line, while accounting for the fact that the first
    -- bits of rest must fit as well (until the first possibility for a line break within rest).
    -- Any whitespace within the group is treated as "compact".
    -- Return Nothing on failure, i.e. if the group would require a line break
    goGroup :: Doc -> Doc -> StateT St Maybe [Text]
    -- In general groups are never empty as empty groups are removed in `fixup`, however this also
    -- gets called for pre and post of priority groups, which may be empty.
    goGroup [] _ = pure []
    goGroup grp rest = StateT $ \(cc, ci) ->
      if cc == 0
        then
          let -- We know that the last printed character was a line break (cc == 0),
              -- therefore drop any leading whitespace within the group to avoid duplicate newlines
              grp' = case head grp of
                Spacing _ -> tail grp
                Group ann ((Spacing _) : inner) -> Group ann inner : tail grp
                _ -> grp
              (nl, off) = nextIndent grp'

              indentWillIncrease = if fst (nextIndent rest) > lineNL then 2 else 0
                where
                  lastLineNL = snd $ NonEmpty.head ci
                  lineNL = lastLineNL + (if nl > lastLineNL then 2 else 0)
          in fits indentWillIncrease (tw - firstLineWidth rest) grp'
              <&> \t -> runState (putText nl off t) (cc, ci)
        else
          let indentWillIncrease = if fst (nextIndent rest) > lineNL then 2 else 0
                where
                  lineNL = snd $ NonEmpty.head ci
          in fits (indentWillIncrease - cc) (tw - cc - firstLineWidth rest) grp
              <&> \t -> ([t], (cc + textWidth t, ci))
