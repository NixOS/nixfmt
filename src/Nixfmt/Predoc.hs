{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE DeriveFoldable, DeriveFunctor, FlexibleInstances,
             OverloadedStrings, StandaloneDeriving #-}

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
import Data.Text as Text (Text, concat, length, pack, replicate)

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
    -- The boolean arguments determine how to handle whitespace directly before/after the
    -- group or at the start/end of the group. By default (False), it gets pulled out the
    -- group, which is what you want in most cases. If set to True,
    -- whitespace before/after the group will be pulled in instead.
    = Group Bool Bool
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

text :: Text -> Doc
text "" = []
text t  = [Text t]

-- | Group document elements together (see Node Group documentation)
-- Any whitespace at the start of the group will get pulled out in front of it.
group :: Pretty a => a -> Doc
group = pure . Node (Group False False) . pretty

-- | Group document elements together (see Node Group documentation)
-- Any whitespace directly before and/or after the group will be pulled into it.
-- Use with caution, and only in situations where you control the surroundings of
-- that group. Especially, never use as a top-level element of a `pretty` instance,
-- or you'll get some *very* confusing bugs …
group' :: Pretty a => Bool -> Bool -> a -> Doc
group' pre post = pure . Node (Group pre post) . pretty

-- | @nest n doc@ sets the indentation for lines in @doc@ to @n@ more than the
-- indentation of the part before it. This is based on the actual indentation of
-- the line, rather than the indentation it should have used: If multiple
-- indentation levels start on the same line, only the last indentation level
-- will be applied on the next line. This prevents unnecessary nesting.
nest :: Int -> Doc -> Doc
nest level = pure . Node (Nest level)

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

isSpacing :: DocE -> Bool
isSpacing (Spacing _) = True
isSpacing _           = False

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = fmap reverse . span p . reverse

-- | Fix up a Doc in multiple stages:
-- - First, all spacings are moved out of Groups and Nests and empty Groups and
--   Nests are removed.
-- - Now, all consecutive Spacings are ensured to be in the same list, so each
--   sequence of Spacings can be merged into a single one.
-- - Finally, Spacings right before a Nest should be moved inside in order to
--   get the right indentation.
fixup :: Doc -> Doc
fixup = moveLinesIn . mergeLines . concatMap moveLinesOut

moveLinesOut :: DocE -> Doc
moveLinesOut (Node ann xs) =
    let movedOut     = concatMap moveLinesOut xs
        (pre, rest)  = span isSpacing movedOut
        (post, body) = spanEnd isSpacing rest
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

mergeLines :: Doc -> Doc
mergeLines []                           = []
mergeLines (Spacing a : Spacing b : xs) = mergeLines $ Spacing (mergeSpacings a b) : xs
mergeLines (Text a : Text b : xs)       = mergeLines $ Text (a <> b) : xs
mergeLines (Node ann xs : ys)           = Node ann (mergeLines xs) : mergeLines ys
mergeLines (x : xs)                     = x : mergeLines xs

moveLinesIn :: Doc -> Doc
moveLinesIn [] = []
-- Move space before Nest in
moveLinesIn (Spacing l : Node (Nest level) xs : ys) =
    moveLinesIn ((Node (Nest level) (Spacing l : xs)) : ys)
-- Move space before (Group True _) in
moveLinesIn (Spacing l : Node ann@(Group True _) xs : ys) =
    moveLinesIn ((Node ann (Spacing l : xs)) : ys)
-- Move space after (Group _ True) in
moveLinesIn (Node ann@(Group _ True) xs : Spacing l : ys) =
    moveLinesIn ((Node ann (xs ++ [Spacing l])) : ys)

moveLinesIn (Node ann xs : ys) =
    Node ann (moveLinesIn xs) : moveLinesIn ys

moveLinesIn (x : xs) = x : moveLinesIn xs

layout :: Pretty a => Int -> a -> Text
layout w = layoutGreedy w . fixup . pretty

-- 1. Move and merge Spacings.
-- 2. Convert Softlines to Grouped Lines and Hardspaces to Texts.
-- 3. For each Text or Group, try to fit as much as possible on a line
-- 4. For each Group, if it fits on a single line, render it that way.
-- 5. If not, convert lines to hardlines and unwrap the group

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

-- | Check if the first line in a list of documents fits a target width given
-- a maximum width, without breaking up groups.
firstLineFits :: Int -> Int -> Doc -> Bool
firstLineFits targetWidth maxWidth docs = go maxWidth docs
    where go c _ | c < 0                = False
          go c []                       = maxWidth - c <= targetWidth
          go c (Text t : xs)            = go (c - textWidth t) xs
          go c (Spacing Hardspace : xs) = go (c - 1) xs
          go c (Spacing _ : _)          = maxWidth - c <= targetWidth
          go c (Node (Group _ _) ys : xs)     =
              case fits (c - firstLineWidth xs) ys of
                   Nothing -> go c (ys ++ xs)
                   Just t  -> go (c - textWidth t) xs

          go c (Node _ ys : xs)         = go c (ys ++ xs)

-- | A document element with target indentation
data Chunk = Chunk Int DocE

-- | Create `n` newlines and `i` spaces
indent :: Int -> Int -> Text
indent n i = Text.replicate n "\n" <> Text.replicate i " "

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
layoutGreedy tw doc = Text.concat $ go 0 0 [Chunk 0 $ Node (Group False False) doc]
    where go :: Int -> Int -> [Chunk] -> [Text]
          go _ _ [] = []
          go cc ci (Chunk ti x : xs) = case x of
            Text t               -> t : go (cc + textWidth t) ci xs

            -- This code treats whitespace as "expanded"
            Spacing Break        -> indent 1 ti : go ti ti xs
            Spacing Space        -> indent 1 ti : go ti ti xs
            Spacing Hardspace    -> " "         : go (cc + 1) ci xs
            Spacing Hardline     -> indent 1 ti : go ti ti xs
            Spacing Emptyline    -> indent 2 ti : go ti ti xs
            Spacing (Newlines n) -> indent n ti : go ti ti xs

            Spacing Softbreak
              | firstLineFits (tw - cc) (tw - ti) (map unChunk xs)
                                 -> go cc ci xs
              | otherwise        -> indent 1 ti : go ti ti xs

            Spacing Softspace
              | firstLineFits (tw - cc - 1) (tw - ti) (map unChunk xs)
                                 -> " " : go (cc + 1) ci xs
              | otherwise        -> indent 1 ti : go ti ti xs

            Node (Nest l) ys     -> go cc ci $ map (Chunk (ti + l)) ys ++ xs
            Node Base ys         -> go cc ci $ map (Chunk ci) ys ++ xs
            Node (Group _ _) ys    ->
                -- Does the group (plus whatever comes after it on that line) fit in one line?
                -- This is where treating whitespace as "compact" happens
                case fits (tw - cc - firstLineWidth (map unChunk xs)) ys of
                     -- Dissolve the group by mapping its members to the target indentation
                     -- This also implies that whitespace in there will now be rendered "expanded"
                     Nothing     -> go cc ci $ map (Chunk ti) ys ++ xs
                     Just t      -> t : go (cc + textWidth t) ci xs
