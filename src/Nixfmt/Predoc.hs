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
    , sepBy
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
import Data.Functor ((<&>))
import Data.Maybe (isNothing, fromMaybe)
import Data.Text as Text (Text, concat, length, pack, replicate, strip)
import GHC.Stack (HasCallStack)
-- import Debug.Trace (traceShow)
import Control.Monad (guard)
import Control.Applicative ((<|>))

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
    -- If it does not fit on one line, but pre does, but prio doesn't, then only expand prio
    -- In all other cases, including when only pre and prio fit into one line, fully expand the group.
    -- Groups containing multiple priority groups are not supported at the momen.
    = Group Bool
    -- | Node (Nest n) doc indicates all line starts in doc should be indented
    -- by n more spaces than the surrounding Base.
    | Nest Int
    -- | Node Base doc sets the base indentation that Nests should be relative
    -- to to the indentation of the line where the Base starts.
    | Base
    deriving (Show, Eq)

-- | Single document element. Documents are modeled as lists of these elements
-- in order to make concatenation simple.
data DocE
    = Text Text
    | Spacing Spacing
    | Node DocAnn Doc
    deriving (Show, Eq)

type Doc = [DocE]

class Pretty a where
    pretty :: a -> Doc

instance Pretty Text where
    pretty = pure . Text

instance Pretty String where
    pretty = pure . Text . pack

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
text t  = [Text t]

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
nest :: HasCallStack => Int -> Doc -> Doc
nest level x = pure . Node (Nest level) $
    if x /= [] && (isSoftSpacing (head x) || isSoftSpacing (last x)) then
       error $ "nest should not start or end with whitespace; " <> show x
    else
        x

base :: Doc -> Doc
base = pure . Node Base

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

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = fmap reverse . span p . reverse

-- | Fix up a Doc in multiple stages:
-- - First, some spacings are moved out of Groups and Nests and empty Groups and
--   Nests are removed.
-- - Merge consecutive spacings. When merging across group/nest boundaries, the merged
--   spacing will be on the "inside" (part of the group).
--   - This may move hard spacing in, so we need to move them out again
fixup :: Doc -> Doc
fixup = concatMap moveLinesOut . mergeLines' . mergeLines . concatMap moveLinesOut

moveLinesOut :: DocE -> Doc
moveLinesOut (Node ann xs) =
    let movedOut     = concatMap moveLinesOut xs
        (pre, rest)  = span isHardSpacing movedOut
        (post, body) = spanEnd isHardSpacing rest
    in case body of
            [] -> pre ++ post
            _  -> pre ++ (Node ann body : post)

moveLinesOut x = [x]

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

-- Merge whitespace and text elements across the document, but not across Node boundaries.
-- After running, any nodes are guaranteed to start/end with at most one whitespace element respectively.
mergeLines :: Doc -> Doc
mergeLines []                           = []
mergeLines (Spacing a : Spacing b : xs) = mergeLines $ Spacing (mergeSpacings a b) : xs
mergeLines (Text a : Text b : xs)       = mergeLines $ Text (a <> b) : xs
mergeLines (Node ann xs : ys)           = Node ann (mergeLines xs) : mergeLines ys
mergeLines (x : xs)                     = x : mergeLines xs

startsWithWhitespace :: Doc -> Bool
startsWithWhitespace (s : _) | isSoftSpacing s = True
startsWithWhitespace ((Node _ inner) : _) = startsWithWhitespace inner
startsWithWhitespace _ = False

endsWithWhitespace :: Doc -> Bool
endsWithWhitespace (s : []) | isSoftSpacing s = True
endsWithWhitespace ((Node _ inner) : []) = endsWithWhitespace inner
endsWithWhitespace (_ : xs) = endsWithWhitespace xs
endsWithWhitespace _ = False

-- Merge whitespace across group borders
mergeLines' :: Doc -> Doc
mergeLines' [] = []
-- Merge things that got moved together
mergeLines' (Spacing a : Spacing b : xs) = mergeLines' $ Spacing (mergeSpacings a b) : xs
-- Move spacing in front of groups in if they can be merged
mergeLines' (Spacing a : Node ann (xs) : ys) | startsWithWhitespace xs =
    mergeLines' $ Node ann (Spacing a : xs) : ys
-- Merge spacings after groups in if they can be merged
mergeLines' (Node ann xs : Spacing a : ys) | endsWithWhitespace xs =
    mergeLines' $ Node ann (xs ++ [Spacing a]) : ys
mergeLines' (Node ann xs : ys) =
    Node ann (mergeLines' xs) : mergeLines' ys
mergeLines' (x : xs) = x : mergeLines' xs

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
fits c (x:xs) = case x of
    Text t               -> (t<>) <$> fits (c - textWidth t) xs
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
firstLineWidth (Text t : xs)            = textWidth t + firstLineWidth xs
firstLineWidth (Spacing Hardspace : xs) = 1 + firstLineWidth xs
firstLineWidth (Spacing _ : _)          = 0
firstLineWidth (Node _ xs : ys)         = firstLineWidth (xs ++ ys)

-- | Check if the first line in a document fits a target width given
-- a maximum width, without breaking up groups.
firstLineFits :: Int -> Int -> Doc -> Bool
firstLineFits targetWidth maxWidth docs = go maxWidth docs
    where go c _ | c < 0                = False
          go c []                       = maxWidth - c <= targetWidth
          go c (Text t : xs)            = go (c - textWidth t) xs
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
layoutGreedy tw doc = Text.concat $ go 0 0 [Chunk 0 $ Node (Group False) doc]
    where go :: Int -> Int -> [Chunk] -> [Text]
          go _ _ [] = []
          go cc ci (Chunk ti x : xs) =
            let
                needsIndent = (cc == 0)
                -- next column, if we print some non-whitespace characters
                nc = if needsIndent then ti else cc
                -- Start of line indentation, if necessary
                lineStart = if needsIndent then indent ti else ""
            in
            case x of
            Text t               -> lineStart : t : go (nc + textWidth t) ci xs

            -- This code treats whitespace as "expanded"
            -- A new line resets the column counter and sets the target indentation as current indentation
            Spacing Break        -> newlines 1 : go 0 ti xs
            Spacing Space        -> newlines 1 : go 0 ti xs
            Spacing Hardspace    -> " "        : go (cc + 1) ci xs
            Spacing Hardline     -> newlines 1 : go 0 ti xs
            Spacing Emptyline    -> newlines 2 : go 0 ti xs
            Spacing (Newlines n) -> newlines n : go 0 ti xs

            Spacing Softbreak
              | firstLineFits (tw - nc) (tw - ti) (map unChunk xs)
                                 ->              go cc ci xs
              | otherwise        -> newlines 1 : go 0 ti xs

            Spacing Softspace
              | firstLineFits (tw - nc - 1) (tw - ti) (map unChunk xs)
                                 -> " "        : go (cc + 1) ci xs
              | otherwise        -> newlines 1 : go 0 ti xs

            Node (Nest l) ys     -> go cc (if needsIndent then ti + l else ci) $ map (Chunk (ti + l)) ys ++ xs
            Node Base ys         -> go cc ci $ map (Chunk ci) ys ++ xs
            Node (Group _) ys    ->
                let
                    -- Does the group (plus whatever comes after it on that line) fit in one line?
                    -- This is where treating whitespace as "compact" happens
                    handleGroup :: Doc -> [Chunk] -> Maybe [Text]
                    handleGroup pre post =
                        if needsIndent then
                            let i = ti + firstLineIndent pre in
                            fits (tw - i - firstLineWidth (map unChunk post)) pre
                            <&> \t -> indent i : t : go (i + textWidth t) ci post
                        else
                            fits (tw - cc - firstLineWidth (map unChunk post)) pre
                            <&> \t -> t : go (cc + textWidth t) ci post
                in
                -- Try to fit the entire group first
                handleGroup ys xs
                -- If that fails, check whether the group contains any priority groups as its children and try to expand them first
                <|> do
                    -- Split up on the first priority group
                    (pre, prio : post) <- Just (break isPriorityGroup ys)
                    -- Make sure to exclude the case where pre and prio fit onto the line but not post.
                    -- This would look weird and also not be true to the intended semantics for priority groups.
                    guard . isNothing $ handleGroup (pre ++ [prio]) (Chunk ti (Node (Group False) post) : xs)
                    -- Try to fit pre onto one line (with prio expanded, also need to re-group post)
                    handleGroup pre ([Chunk ti prio, Chunk ti (Node (Group False) post)] ++ xs)
                -- Otherwise, dissolve the group by mapping its members to the target indentation
                -- This also implies that whitespace in there will now be rendered "expanded"
                & fromMaybe (go cc ci $ map (Chunk ti) ys ++ xs)
