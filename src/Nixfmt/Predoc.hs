{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}

-- | This module implements a layer around the prettyprinter package, making it
-- easier to use.
module Nixfmt.Predoc
    ( text
    , comment
    , trailing
    , sepBy
    , surroundWith
    , hcat
    , base
    , group
    , group'
    , nest
    , softline'
    , line'
    , softline
    , line
    , hardspace
    , hardline
    , emptyline
    , newline
    , Doc
    , Pretty
    , pretty
    , fixup
    , layout
    , textWidth
    ) where

import Data.List (intersperse)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, concat, length, replicate, strip)
import GHC.Stack (HasCallStack)
-- import Debug.Trace (traceShow, traceShowId)
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, StateT, StateT(..), mapStateT, state, runState, evalState, get, put)

-- | Sequential Spacings are reduced to a single Spacing by taking the maximum.
-- This means that e.g. a Space followed by an Emptyline results in just an
-- Emptyline.
data Spacing
    =
    -- | Line break or nothing (soft)
    Softbreak
    -- | Line break or nothing
    | Break
    -- | Always a space
    | Hardspace
    -- | Line break or space (soft)
    | Softspace
    -- | Line break or space
    | Space
    -- | Always a line break
    | Hardline
    -- | Two line breaks
    | Emptyline
    -- | n line breaks
    | Newlines Int
    deriving (Show, Eq, Ord)

data DocAnn
    -- | Node Group docs indicates either all or none of the Spaces and Breaks
    -- in docs should be converted to line breaks. This does not affect softlines,
    -- those will be expanded only as necessary and with a lower priority.
    --
    -- The boolean argument makes a group a "high priority" group. You should almost
    -- never need this (it was introduced purely to accomodate for some Application special
    -- handling). Groups containing priority groups are treated as having three segments:
    -- pre, prio and post.
    -- If any group contains a priority group, the following happens:
    -- If it entirely fits on one line, render on one line (as usual).
    -- If it does not fit on one line, but pre and post do when prio is expanded, then try that.
    -- In all other cases, fully expand the group.
    -- Groups containing multiple priority groups are not supported at the moment.
    = Group Bool
    -- | Node (Nest n) doc indicates all line starts in doc should be indented
    -- by n more spaces than the surrounding Base.
    | Nest Int
    -- | Node Base doc sets the base indentation that Nests should be relative
    -- to to the indentation of the line where the Base starts.
    | Base
    deriving (Show, Eq)

-- Comments do not count towards some line length limits
-- Trailing tokens have the property that they will only exist in expanded groups, and "swallowed" in compact groups
data TextAnn = Regular | Comment | Trailing
    deriving (Show, Eq)

-- | Single document element. Documents are modeled as lists of these elements
-- in order to make concatenation simple.
data DocE
    = Text TextAnn Text
    | Spacing Spacing
    | Node DocAnn Doc
    deriving (Show, Eq)

type Doc = [DocE]

class Pretty a where
    pretty :: a -> Doc

instance Pretty Doc where
    pretty = id

instance Pretty a => Pretty (Maybe a) where
    pretty Nothing  = mempty
    pretty (Just x) = pretty x

instance (Pretty a, Pretty b) => Pretty (a, b) where
    pretty (a, b) = pretty a <> pretty b

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
    pretty (a, b, c) = pretty a <> pretty b <> pretty c

text :: Text -> Doc
text "" = []
text t  = [Text Regular t]

comment :: Text -> Doc
comment "" = []
comment t  = [Text Comment t]

-- Text tokens that are only needed in expanded groups
trailing :: Text -> Doc
trailing "" = []
trailing t = [Text Trailing t]

-- | Group document elements together (see Node Group documentation)
-- Must not contain non-hard whitespace (e.g. line, softline' etc.) at the start of the end.
-- Use group' for that instead if you are sure of what you are doing.
group :: HasCallStack => Pretty a => a -> Doc
group x = pure . Node (Group False) $
    if p /= [] && (isSoftSpacing (head p) || isSoftSpacing (last p)) then
        error $ "group should not start or end with whitespace, use `group'` if you are sure; " <> show p
    else
        p
    where p = pretty x

-- | Group document elements together (see Node Group documentation)
-- Is allowed to start or end with any kind of whitespace.
-- Use with caution, and only in situations where you control the surroundings of
-- that group. Especially, never use as a top-level element of a `pretty` instance,
-- or you'll get some *very* confusing bugs …
--
-- Also allows to create priority groups (see Node Group documentation)
group' :: Pretty a => Bool -> a -> Doc
group' prio = pure . Node (Group prio) . pretty

-- | @nest n doc@ sets the indentation for lines in @doc@ to @n@ more than the
-- indentation of the part before it. This is based on the actual indentation of
-- the line, rather than the indentation it should have used: If multiple
-- indentation levels start on the same line, only the last indentation level
-- will be applied on the next line. This prevents unnecessary nesting.
nest :: HasCallStack => Pretty a => Int -> a -> Doc
nest level x = pure . Node (Nest level) $
    if x' /= [] && (isSoftSpacing (head x') || isSoftSpacing (last x')) then
       error $ "nest should not start or end with whitespace; " <> show x'
    else
        x'
    where x' = pretty x

base :: Pretty a => a -> Doc
base = pure . Node Base . pretty

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

surroundWith :: Pretty a => Doc -> a -> Doc
surroundWith outside inner = outside <> pretty inner <> outside

sepBy :: Pretty a => Doc -> [a] -> Doc
sepBy separator = mconcat . intersperse separator . map pretty

-- | Concatenate documents horizontally without spacing.
hcat :: Pretty a => [a] -> Doc
hcat = mconcat . map pretty

-- Everything that may change representation depending on grouping
isSoftSpacing :: DocE -> Bool
isSoftSpacing (Spacing Softbreak) = True
isSoftSpacing (Spacing Break) = True
isSoftSpacing (Spacing Softspace) = True
isSoftSpacing (Spacing Space) = True
isSoftSpacing _           = False

-- Everything else
isHardSpacing :: DocE -> Bool
isHardSpacing (Spacing Hardspace) = True
isHardSpacing (Spacing Hardline) = True
isHardSpacing (Spacing Emptyline) = True
isHardSpacing (Spacing (Newlines _)) = True
isHardSpacing _           = False

-- Check if an element is a comment
-- Some comments are nested as nodes with multiple elements.
-- Therefore nodes are counted as comments if they only contain comments or hard spacings
isComment :: DocE -> Bool
isComment (Text Comment _) = True
isComment (Node _ inner) = all (\x -> isComment x || isHardSpacing x) inner
isComment _ = False

--- Manually force a group to its compact layout, by replacing all relevant whitespace.
--- Does recurse into inner groups.
unexpandSpacing :: Doc -> Doc
unexpandSpacing [] = []
unexpandSpacing ((Spacing Space):xs) = Spacing Hardspace : unexpandSpacing xs
unexpandSpacing ((Spacing Softspace):xs) = Spacing Hardspace : unexpandSpacing xs
unexpandSpacing ((Spacing Break):xs) = unexpandSpacing xs
unexpandSpacing ((Spacing Softbreak):xs) = unexpandSpacing xs
unexpandSpacing (s@(Spacing _):xs) = s : unexpandSpacing xs
unexpandSpacing (x:xs) = x : unexpandSpacing xs

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = fmap reverse . span p . reverse

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
-- Merge consecutive texts
fixup (Text ann a : Text ann' b : xs) | ann == ann' = fixup $ Text ann (a <> b) : xs
-- Handle node, with stuff in front of it to potentially merge with
fixup (a@(Spacing _) : Node ann xs : ys) =
    let
        moveComment = case ann of { Nest _ -> False; _ -> True }
        -- Recurse onto xs, split out leading and trailing whitespace into pre and post.
        -- For the leading side, also move out comments out of groups, they are kinda the same thing
        -- (We could move out trailing comments too but it would make no difference)
        (pre, rest)  = span (\x -> isHardSpacing x || (moveComment && isComment x)) $ fixup xs
        (post, body) = spanEnd isHardSpacing rest
    in if null body then
        -- Dissolve empty node
        fixup $ (a : pre) ++ post ++ ys
    else
        fixup (a : pre) ++ [Node ann body] ++ fixup (post ++ ys)
-- Handle node, almost the same thing as above
fixup (Node ann xs : ys) =
    let
        moveComment = case ann of { Nest _ -> False; _ -> True }
        (pre, rest)  = span (\x -> isHardSpacing x || (moveComment && isComment x)) $ fixup xs
        (post, body) = spanEnd isHardSpacing rest
    in if null body then
        fixup $ pre ++ post ++ ys
    else
        fixup pre ++ [Node ann body] ++ fixup (post ++ ys)
fixup (x : xs) = x : fixup xs

mergeSpacings :: Spacing -> Spacing -> Spacing
mergeSpacings x y | x > y               = mergeSpacings y x
mergeSpacings Break        Softspace    = Space
mergeSpacings Break        Hardspace    = Space
mergeSpacings Softbreak    Hardspace    = Softspace
mergeSpacings (Newlines x) (Newlines y) = Newlines (x + y)
mergeSpacings Emptyline    (Newlines x) = Newlines (x + 2)
mergeSpacings Hardspace    (Newlines x) = Newlines x
mergeSpacings _            (Newlines x) = Newlines (x + 1)
mergeSpacings _            y            = y

layout :: Pretty a => Int -> a -> Text
layout w = (<>"\n") . Text.strip . layoutGreedy w . fixup . pretty

-- 1. Move and merge Spacings.
-- 2. Convert Softlines to Grouped Lines and Hardspaces to Texts.
-- 3. For each Text or Group, try to fit as much as possible on a line
-- 4. For each Group, if it fits on a single line, render it that way.
-- 5. If not, convert lines to hardlines and unwrap the group

isPriorityGroup :: DocE -> Bool
isPriorityGroup (Node (Group True) _) = True
isPriorityGroup _ = False

-- | To support i18n, this function needs to be patched.
textWidth :: Text -> Int
textWidth = Text.length

-- | Attempt to fit a list of documents in a single line of a specific width.
fits :: Int -> Doc -> Maybe Text
fits c _ | c < 0 = Nothing
fits _ [] = Just ""
-- This case is impossible in the input thanks to fixup, but may happen
-- due to our recursion on nodes below
fits c (Spacing a:Spacing b:xs) = fits c (Spacing (mergeSpacings a b):xs)
fits c (x:xs) = case x of
    Text Regular t       -> (t<>) <$> fits (c - textWidth t) xs
    Text Comment t       -> (t<>) <$> fits c xs
    Text Trailing _      -> fits c xs
    Spacing Softbreak    -> fits c xs
    Spacing Break        -> fits c xs
    Spacing Softspace    -> (" "<>) <$> fits (c - 1) xs
    Spacing Space        -> (" "<>) <$> fits (c - 1) xs
    Spacing Hardspace    -> (" "<>) <$> fits (c - 1) xs
    Spacing Hardline     -> Nothing
    Spacing Emptyline    -> Nothing
    Spacing (Newlines _) -> Nothing
    Node _ ys            -> fits c $ ys ++ xs

-- | Find the width of the first line in a list of documents, using target
-- width 0, which always forces line breaks when possible.
firstLineWidth :: Doc -> Int
firstLineWidth []                       = 0
firstLineWidth (Text Comment _ : xs)    = firstLineWidth xs
firstLineWidth (Text _ t : xs)          = textWidth t + firstLineWidth xs
-- This case is impossible in the input thanks to fixup, but may happen
-- due to our recursion on nodes below
firstLineWidth (Spacing a : Spacing b : xs) = firstLineWidth (Spacing (mergeSpacings a b):xs)
firstLineWidth (Spacing Hardspace : xs) = 1 + firstLineWidth xs
firstLineWidth (Spacing _ : _)          = 0
firstLineWidth (Node _ xs : ys)         = firstLineWidth $ xs ++ ys

-- | Check if the first line in a document fits a target width given
-- a maximum width, without breaking up groups.
firstLineFits :: Int -> Int -> Doc -> Bool
firstLineFits targetWidth maxWidth docs = go maxWidth docs
    where go c _ | c < 0                = False
          go c []                       = maxWidth - c <= targetWidth
          go c (Text Regular t : xs)    = go (c - textWidth t) xs
          go c (Text _ _ : xs)          = go c xs
          -- This case is impossible in the input thanks to fixup, but may happen
          -- due to our recursion on nodes below
          go c (Spacing a : Spacing b : xs) = go c $ Spacing (mergeSpacings a b) : xs
          go c (Spacing Hardspace : xs) = go (c - 1) xs
          go c (Spacing _ : _)          = maxWidth - c <= targetWidth
          go c (Node (Group _) ys : xs)     =
              case fits (c - firstLineWidth xs) ys of
                   Nothing -> go c (ys ++ xs)
                   Just t  -> go (c - textWidth t) xs

          go c (Node _ ys : xs)         = go c (ys ++ xs)

-- Calculate the amount of indentation until the first token
firstLineIndent :: Doc -> Int
firstLineIndent ((Node (Nest n) xs) : _) = n + firstLineIndent xs
firstLineIndent ((Node _ xs) : _) = firstLineIndent xs
firstLineIndent _ = 0

-- | A document element with target indentation
data Chunk = Chunk Int DocE

-- | Create `n` newlines
newlines :: Int -> Text
newlines n = Text.replicate n "\n"

-- | Create `n` spaces
indent :: Int -> Text
indent n = Text.replicate n " "

unChunk :: Chunk -> DocE
unChunk (Chunk _ doc) = doc

-- tw   Target Width
-- cc   Current Column
-- ci   Current Indentation
-- ti   Target Indentation
--        an indent only changes the target indentation at first.
--        Only for the tokens starting on the next line the current
--        indentation will match the target indentation.
layoutGreedy :: Int -> Doc -> Text
layoutGreedy tw doc = Text.concat $ evalState (go [Chunk 0 $ Node (Group False) doc] []) (0, 0)
    where
    -- All state is (cc, ci)

    -- First argument: chunks to render
    -- Second argument: lookahead of following chunks, not rendered
    go :: [Chunk] -> [Chunk] -> State (Int, Int) [Text]
    go [] _ = return []
    go (x:xs) ys = do { t <- goOne x (xs ++ ys); ts <- go xs ys; return (t ++ ts) }

    -- First argument: chunk to render. This will recurse into nests/groups if the chunk is one.
    -- Second argument: lookahead of following chunks
    goOne :: Chunk -> [Chunk] -> State (Int, Int) [Text]
    goOne (Chunk ti x) xs = get >>= \(cc, ci) ->
        let
            xs' = map unChunk xs

            -- The last printed character was a line break
            needsIndent = (cc == 0)
            -- Start of line indentation, if necessary
            lineStart = if needsIndent then indent ti else ""

            -- Some state helpers
            putText ts = put (cc + sum (map textWidth ts), ci) $> ts
            putNL = put (0, ti)
        in case x of
        Text _ t             -> putText [lineStart, t]

        -- This code treats whitespace as "expanded"
        -- A new line resets the column counter and sets the target indentation as current indentation
        Spacing sp ->
            -- We know that the last printed character was a line break (cc == 0),
            -- therefore drop any leading whitespace within the group to avoid duplicate newlines
            if needsIndent then
                pure []
            else case sp of
                Break        -> putNL $> [newlines 1]
                Space        -> putNL $> [newlines 1]
                Hardspace    -> putText [" "]
                Hardline     -> putNL $> [newlines 1]
                Emptyline    -> putNL $> [newlines 2]
                (Newlines n) -> putNL $> [newlines n]
                Softbreak
                    | firstLineFits (tw - cc + ci) (tw - ti) xs'
                                            -> pure []
                    | otherwise             -> putNL $> [newlines 1]
                Softspace
                    | firstLineFits (tw - cc + ci - 1) (tw - ti) xs'
                                            -> putText [" "]
                    | otherwise             -> putNL $> [newlines 1]

        Node (Nest l) ys     -> put (cc, if cc == 0 then ti + l else ci) >> go (map (Chunk (ti + l)) ys) xs
        Node Base ys         -> go (map (Chunk ci) ys) xs
        Node (Group _) ys    ->
            let
                -- fromMaybe lifted to (StateT s Maybe)
                fromMaybeState :: State s a -> StateT s Maybe a -> State s a
                fromMaybeState l r = state $ \s -> fromMaybe (runState l s) (runStateT r s)
            in
            -- Try to fit the entire group first
            goGroup ti ys xs'
            -- If that fails, check whether the group contains a priority group within its children and try to expand that first
            <|> do
                -- Split up on the first priority group, if present
                -- Note that the pattern on prio is infallible as per isPriorityGroup
                (pre, (Node (Group True) prio) : post) <- pure $ (break isPriorityGroup ys)
                -- Try to fit pre onto one line
                preRendered <- goGroup ti pre (prio ++ post ++ xs')
                -- Render prio expanded
                -- We know that post will be rendered compact. So we tell the renderer that by manually removing all
                -- line breaks in post here. Otherwise we might get into awkward the situation where pre and prio are put
                -- onto the one line, all three obviously wouldn't fit.
                prioRendered <- mapStateT (Just . runIdentity) $
                    go (map (Chunk ti) prio) (map (Chunk ti) (unexpandSpacing post) ++ xs)
                -- Try to render post onto one line
                postRendered <- goGroup ti post xs'
                -- If none of these failed, put together and return
                return $ (preRendered ++ prioRendered ++ postRendered)
            -- Otherwise, dissolve the group by mapping its members to the target indentation
            -- This also implies that whitespace in there will now be rendered "expanded".
            & fromMaybeState (go (map (Chunk ti) ys) xs)

    -- Try to fit the group onto a single line, while accounting for the fact that the first
    -- bits of rest must fit as well (until the first possibility for a line break within rest).
    -- Any whitespace within the group is treated as "compact".
    -- Return Nothing on failure, i.e. if the group would require a line break
    goGroup :: Int -> Doc -> Doc -> StateT (Int, Int) Maybe [Text]
    -- In general groups are never empty as empty groups are removed in `fixup`, however this also
    -- gets called for pre and post of priority groups, which may be empty.
    goGroup _ [] _ = pure []
    goGroup ti grp rest = StateT $ \(cc, ci) ->
        if cc == 0 then
            let
                -- We know that the last printed character was a line break (cc == 0),
                -- therefore drop any leading whitespace within the group to avoid duplicate newlines
                grp' = case head grp of
                    Spacing _ -> tail grp
                    Node ann@(Group _) ((Spacing _) : inner) -> (Node ann inner) : tail grp
                    _ -> grp
                i = ti + firstLineIndent grp'
            in
            fits (tw - firstLineWidth rest) grp'
            <&> \t -> ([indent i, t], (i + textWidth t, ci))
        else
            fits (tw + (ci - cc) - firstLineWidth rest) grp
            <&> \t -> ([t], (cc + textWidth t, ci))
