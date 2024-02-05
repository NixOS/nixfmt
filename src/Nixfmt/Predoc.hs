{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE FlexibleInstances, OverloadedStrings, LambdaCase #-}

module Nixfmt.Predoc
    ( text
    , comment
    , trailingComment
    , trailing
    , sepBy
    , surroundWith
    , hcat
    , group
    , group'
    , nest
    , offset
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
    , unexpandSpacing'
    , layout
    , textWidth
    ) where

import Data.List (intersperse)
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.Functor.Identity (runIdentity)
import Data.Bifunctor (second)
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
    deriving (Show, Eq)

-- Comments do not count towards some line length limits
-- Trailing tokens have the property that they will only exist in expanded groups, and "swallowed" in compact groups
-- Trailing comments are like comments, but marked differently for special treatment further down the line
-- (The difference is that trailing comments are guaranteed to be single "# text" tokens, while all other comments
-- may be composite and multi-line)
data TextAnn = Regular | Comment | TrailingComment | Trailing
    deriving (Show, Eq)

-- | Single document element. Documents are modeled as lists of these elements
-- in order to make concatenation simple.
data DocE =
    -- indent level, offset, kind, text
    Text Int Int TextAnn Text
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
text t  = [Text 0 0 Regular t]

comment :: Text -> Doc
comment "" = []
comment t  = [Text 0 0 Comment t]

-- Comment at the end of a line
trailingComment :: Text -> Doc
trailingComment "" = []
trailingComment t  = [Text 0 0 TrailingComment t]

-- Text tokens that are only needed in expanded groups
trailing :: Text -> Doc
trailing "" = []
trailing t = [Text 0 0 Trailing t]

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

-- | @nest doc@ declarse @doc@ to have a higher indentation level
-- than before. Not all nestings actually result in indentation changes,
-- this will be calculated automatically later on. As a rule of thumb:
-- Multiple indentation levels on one line will be compacted and only result in a single
-- bump for the next line. This prevents excessive indentation.
nest :: Pretty a => a -> Doc
nest x = go $ pretty x
    where
    go (Text i o ann t : rest) = (Text (i + 2) o ann t) : go rest
    go (Node ann inner : rest) = (Node ann (go inner)) : go rest
    go (spacing : rest) = spacing : go rest
    go [] = []

-- This is similar to nest, however it circumvents the "smart" rules that usually apply.
-- This should only be useful to manage the indentation within indented strings.
offset :: Pretty a => Int -> a -> Doc
offset level x = go $ pretty x
    where
    go (Text i o ann t : rest) = (Text i (o + level) ann t) : go rest
    go (Node ann inner : rest) = (Node ann (go inner)) : go rest
    go (spacing : rest) = spacing : go rest
    go [] = []

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
isComment (Text _ _ Comment _) = True
isComment (Text _ _ TrailingComment _) = True
isComment (Node _ inner) = all (\x -> isComment x || isHardSpacing x) inner
isComment _ = False

--- Manually force a group to its compact layout, by replacing all relevant whitespace.
--- Does not recurse into inner groups.
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

-- Manually force a group to its compact layout, by replacing all relevant whitespace.
-- Does recurse into inner groups.
-- An optional maximum line length limit may be specified.
-- Fails if the doc contains hardlines or exceeds the length limit
unexpandSpacing' :: Maybe Int -> Doc -> Maybe Doc
unexpandSpacing' (Just n) _ | n < 0 = Nothing
unexpandSpacing' _ [] = Just []
unexpandSpacing' n (txt@(Text _ _ _ t):xs) = (txt :) <$> unexpandSpacing' (n <&> (subtract $ textWidth t)) xs
unexpandSpacing' n (Spacing Hardspace:xs) = (Spacing Hardspace :) <$> unexpandSpacing' (n <&> (subtract 1)) xs
unexpandSpacing' n (Spacing Space:xs) = (Spacing Hardspace :) <$> unexpandSpacing' (n <&> (subtract 1)) xs
unexpandSpacing' n (Spacing Softspace:xs) = (Spacing Hardspace :) <$> unexpandSpacing' (n <&> (subtract 1)) xs
unexpandSpacing' n (Spacing Break:xs) = unexpandSpacing' n xs
unexpandSpacing' n (Spacing Softbreak:xs) = unexpandSpacing' n xs
unexpandSpacing' _ (Spacing _:_) = Nothing
unexpandSpacing' n ((Node _ xs):ys) = unexpandSpacing' n $ xs <> ys

simplifyNode :: DocAnn -> Doc -> Doc
simplifyNode _ [] = []
simplifyNode (Group False) [Node (Group False) body] = body
simplifyNode _ x = x

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
-- Handle node, with stuff in front of it to potentially merge with
fixup (a@(Spacing _) : Node ann xs : ys) =
    let
        -- Recurse onto xs, split out leading and trailing whitespace into pre and post.
        -- For the leading side, also move out comments out of groups, they are kinda the same thing
        -- (We could move out trailing comments too but it would make no difference)
        (pre, rest)  = span (\x -> isHardSpacing x || isComment x) $ fixup xs
        (post, body) = (second $ simplifyNode ann) $ spanEnd isHardSpacing rest
    in if null body then
        -- Dissolve empty node
        fixup $ (a : pre) ++ post ++ ys
    else
        fixup (a : pre) ++ [Node ann body] ++ fixup (post ++ ys)
-- Handle node, almost the same thing as above
fixup (Node ann xs : ys) =
    let
        (pre, rest)  = span (\x -> isHardSpacing x || isComment x) $ fixup xs
        (post, body) = (second $ simplifyNode ann) $ spanEnd isHardSpacing rest
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
-- ni — next indentation. Only used for trailing comment calculations. Set this to the indentation
--      of the next line relative to the current one. So usuall 2 when the indentation level increases, 0 otherwise.
-- c — allowed width
fits :: Int -> Int -> Doc -> Maybe Text
fits _ c _ | c < 0 = Nothing
fits _ _ [] = Just ""
-- This case is impossible in the input thanks to fixup, but may happen
-- due to our recursion on nodes below
fits ni c (Spacing a:Spacing b:xs) = fits ni c (Spacing (mergeSpacings a b):xs)
fits ni c (x:xs) = case x of
    Text _ _ Regular t   -> (t<>) <$> fits (ni - textWidth t) (c - textWidth t) xs
    Text _ _ Comment t   -> (t<>) <$> fits ni c xs
    Text _ _ TrailingComment t | ni == 0 -> ((" " <> t) <>) <$> fits ni c xs
                         | otherwise -> (t<>) <$> fits ni c xs
    Text _ _ Trailing _  -> fits ni c xs
    Spacing Softbreak    -> fits ni c xs
    Spacing Break        -> fits ni c xs
    Spacing Softspace    -> (" "<>) <$> fits (ni - 1) (c - 1) xs
    Spacing Space        -> (" "<>) <$> fits (ni - 1) (c - 1) xs
    Spacing Hardspace    -> (" "<>) <$> fits (ni - 1) (c - 1) xs
    Spacing Hardline     -> Nothing
    Spacing Emptyline    -> Nothing
    Spacing (Newlines _) -> Nothing
    Node _ ys            -> fits ni c $ ys ++ xs

-- | Find the width of the first line in a list of documents, using target
-- width 0, which always forces line breaks when possible.
firstLineWidth :: Doc -> Int
firstLineWidth []                       = 0
firstLineWidth (Text _ _ Comment _ : xs)  = firstLineWidth xs
firstLineWidth (Text _ _ TrailingComment _ : xs) = firstLineWidth xs
firstLineWidth (Text _ _ _ t : xs)        = textWidth t + firstLineWidth xs
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
          go c (Text _ _ Regular t : xs)  = go (c - textWidth t) xs
          go c (Text _ _ _ _ : xs)        = go c xs
          -- This case is impossible in the input thanks to fixup, but may happen
          -- due to our recursion on nodes below
          go c (Spacing a : Spacing b : xs) = go c $ Spacing (mergeSpacings a b) : xs
          go c (Spacing Hardspace : xs) = go (c - 1) xs
          go c (Spacing _ : _)          = maxWidth - c <= targetWidth
          go c (Node (Group _) ys : xs)     =
              case fits 0 (c - firstLineWidth xs) ys of
                   Nothing -> go c (ys ++ xs)
                   Just t  -> go (c - textWidth t) xs

-- Calculate the amount of indentation until the first token
-- This assumes the input to be an unexpanded group at the start of a new line
nextIndent :: Doc -> (Int, Int)
nextIndent ((Text i o _ _) : _) = (i, o)
nextIndent ((Node _ xs) : _) = nextIndent xs
nextIndent (_:xs) = nextIndent xs
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
--   A stack of tuples (realIndent, virtualIndent)
--   This is guaranteed to never be empty, as we start with [(0, 0)] and never go below that.
type St = (Int, [(Int, Int)])

-- tw   Target Width
layoutGreedy :: Int -> Doc -> Text
layoutGreedy tw doc = Text.concat $ evalState (go [Node (Group False) doc] []) (0, [(0, 0)])
    where
    -- Print a given text. If this is the first token on a line, it will
    -- do the appropriate calculations for indentation and print that in addition to the text.
    putText :: Int -> Int -> Text -> State St [Text]
    putText textVI textOffset t = get >>=
        \case
            -- Needs indent, but no more than last line
            (0, indents@((ci, vi):_)) | textVI == vi ->
                go' indents (ci + textOffset)
            -- Needs more indent than last line. We only go up by one level every time
            (0, indents@((ci, vi):_)) | textVI > vi ->
                go' ((ci + 2, textVI):indents) (ci + 2 + textOffset)
            -- Need to go down one or more levels
            -- Just pop from the stack and recurse until the indent matches again
            (0, ((_, vi) : indents@((ci, vi'):_))) | textVI < vi ->
                if textVI < vi' then
                    put (0, indents) >> putText textVI textOffset t
                else
                    go' indents (ci + textOffset)
            -- Does not need indent (not at start of line)
            (cc, indents) ->
                put (cc + textWidth t, indents) $> [t]
        where
            -- Start a new line
            go' indents i = put (textWidth t, indents) $> [indent i, t]

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
    go (x:xs) ys = do { t <- goOne x (xs ++ ys); ts <- go xs ys; return (t ++ ts) }

    -- First argument: chunk to render. This will recurse into nests/groups if the chunk is one.
    -- Second argument: lookahead of following chunks
    goOne :: DocE -> Doc -> State St [Text]
    goOne x xs = get >>= \(cc, indents) ->
        let
            -- The last printed character was a line break
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
        Text _ _ TrailingComment t | cc == 2 && (fst $ nextIndent xs) > lineVI -> putText' [" ", t]
            where lineVI = snd $ head indents
        Text vi off _ t -> putText vi off t

        -- This code treats whitespace as "expanded"
        -- A new line resets the column counter and sets the target indentation as current indentation
        Spacing sp
            -- We know that the last printed character was a line break (cc == 0),
            -- therefore drop any leading whitespace within the group to avoid duplicate newlines
            | needsIndent -> pure []
            | otherwise -> case sp of
                Break        -> putNL 1
                Space        -> putNL 1
                Hardspace    -> putText' [" "]
                Hardline     -> putNL 1
                Emptyline    -> putNL 2
                (Newlines n) -> putNL n
                Softbreak
                    | firstLineFits (tw - cc) tw xs
                                            -> pure []
                    | otherwise             -> putNL 1
                Softspace
                    | firstLineFits (tw - cc - 1) tw xs
                                            -> putText' [" "]
                    | otherwise             -> putNL 1

        Node (Group _) ys    ->
            let
                -- fromMaybe lifted to (StateT s Maybe)
                fromMaybeState :: State s a -> StateT s Maybe a -> State s a
                fromMaybeState l r = state $ \s -> fromMaybe (runState l s) (runStateT r s)
            in
            -- Try to fit the entire group first
            goGroup ys xs
            -- If that fails, check whether the group contains a priority group within its children and try to expand that first
            <|> do
                -- Split up on the first priority group, if present
                -- Note that the pattern on prio is infallible as per isPriorityGroup
                (pre, (Node (Group True) prio) : post) <- pure $ (break isPriorityGroup ys)
                -- Try to fit pre onto one line
                preRendered <- goGroup pre (prio ++ post ++ xs)
                -- Render prio expanded
                -- We know that post will be rendered compact. So we tell the renderer that by manually removing all
                -- line breaks in post here. Otherwise we might get into awkward the situation where pre and prio are put
                -- onto the one line, all three obviously wouldn't fit.
                prioRendered <- mapStateT (Just . runIdentity) $
                    go prio (unexpandSpacing post ++ xs)
                -- Try to render post onto one line
                postRendered <- goGroup post xs
                -- If none of these failed, put together and return
                return $ (preRendered ++ prioRendered ++ postRendered)
            -- Otherwise, dissolve the group by mapping its members to the target indentation
            -- This also implies that whitespace in there will now be rendered "expanded".
            & fromMaybeState (go ys xs)

    -- Try to fit the group onto a single line, while accounting for the fact that the first
    -- bits of rest must fit as well (until the first possibility for a line break within rest).
    -- Any whitespace within the group is treated as "compact".
    -- Return Nothing on failure, i.e. if the group would require a line break
    goGroup :: Doc -> Doc -> StateT St Maybe [Text]
    -- In general groups are never empty as empty groups are removed in `fixup`, however this also
    -- gets called for pre and post of priority groups, which may be empty.
    goGroup [] _ = pure []
    goGroup grp rest = StateT $ \(cc, ci) ->
        if cc == 0 then
            let
                -- We know that the last printed character was a line break (cc == 0),
                -- therefore drop any leading whitespace within the group to avoid duplicate newlines
                grp' = case head grp of
                    Spacing _ -> tail grp
                    Node ann@(Group _) ((Spacing _) : inner) -> (Node ann inner) : tail grp
                    _ -> grp
                (vi, off) = nextIndent grp'

                indentWillIncrease = if fst (nextIndent rest) > lineVI then 2 else 0
                    where
                        lastLineVI = snd $ head ci
                        lineVI = lastLineVI + (if vi > lastLineVI then 2 else 0)
            in
            fits indentWillIncrease (tw - firstLineWidth rest) grp'
            <&> \t -> runState (putText vi off t) (cc, ci)
        else
            let
                indentWillIncrease = if fst (nextIndent rest) > lineVI then 2 else 0
                    where lineVI = snd $ head ci
            in
            fits (indentWillIncrease - cc) (tw - cc - firstLineWidth rest) grp
            <&> \t -> ([t], (cc + textWidth t, ci))
