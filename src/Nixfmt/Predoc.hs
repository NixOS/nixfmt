{-# LANGUAGE DeriveFoldable, DeriveFunctor, FlexibleInstances,
             OverloadedStrings, StandaloneDeriving #-}

-- | This module implements a layer around the prettyprinter package, making it
-- easier to use.
module Nixfmt.Predoc
    ( text
    , sepBy
    , hcat
    , group
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
    , flatten
    , fixup
    , layout
    ) where

import Data.List hiding (group)
import Data.Text as Text (Text, concat, length, pack, replicate)

data Tree a
    = EmptyTree
    | Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show, Functor, Foldable)

-- | Sequential Spacings are reduced to a single Spacing by taking the maximum.
-- This means that e.g. a Space followed by an Emptyline results in just an
-- Emptyline.
data Spacing
    = Softbreak
    | Break
    | Softspace
    | Space
    | Hardspace
    | Hardline
    | Emptyline
    deriving (Show, Eq, Ord)

data Predoc f
    = Text Text
    | Spacing Spacing
    | Newline
    -- | Group predoc indicates either all or none of the Spaces and Breaks in
    -- predoc should be converted to line breaks.
    | Group (f (Predoc f))
    -- | Nest n predoc indicates all line start in predoc should be indented by
    -- n more spaces than the surroundings.
    | Nest Int (f (Predoc f))

deriving instance Show (Predoc Tree)
deriving instance Show (Predoc [])

type Doc = Tree (Predoc Tree)
type DocList = [Predoc []]

class Pretty a where
    pretty :: a -> Doc

instance Pretty Text where
    pretty = Leaf . Text

instance Pretty String where
    pretty = Leaf . Text . pack

instance Pretty Doc where
    pretty = id

instance Pretty a => Pretty (Maybe a) where
    pretty Nothing  = mempty
    pretty (Just x) = pretty x

instance Semigroup (Tree a) where
    left <> right = Node left right

instance Monoid (Tree a) where
    mempty = EmptyTree

text :: Text -> Doc
text = Leaf . Text

group :: Doc -> Doc
group = Leaf . Group

nest :: Int -> Doc -> Doc
nest level = Leaf . Nest level

softline' :: Doc
softline' = Leaf (Spacing Softbreak)

line' :: Doc
line' = Leaf (Spacing Break)

softline :: Doc
softline = Leaf (Spacing Softspace)

line :: Doc
line = Leaf (Spacing Space)

hardspace :: Doc
hardspace = Leaf (Spacing Hardspace)

hardline :: Doc
hardline = Leaf (Spacing Hardline)

emptyline :: Doc
emptyline = Leaf (Spacing Emptyline)

newline :: Doc
newline = Leaf Newline

sepBy :: Pretty a => Doc -> [a] -> Doc
sepBy separator = mconcat . intersperse separator . map pretty

-- | Concatenate documents horizontally without spacing.
hcat :: Pretty a => [a] -> Doc
hcat = mconcat . map pretty

flatten :: Doc -> DocList
flatten = go []
    where go xs (Node x y)           = go (go xs y) x
          go xs EmptyTree            = xs
          go xs (Leaf (Group tree))  = Group (go [] tree) : xs
          go xs (Leaf (Nest l tree)) = Nest l (go [] tree) : xs
          go xs (Leaf (Spacing l))   = Spacing l : xs
          go xs (Leaf Newline)       = Newline : xs
          go xs (Leaf (Text ""))     = xs
          go xs (Leaf (Text t))      = Text t : xs

isSpacing :: Predoc f -> Bool
isSpacing (Spacing _) = True
isSpacing _           = False

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = fmap reverse . span p . reverse

-- | Fix up a DocList in multiple stages:
-- - First, all spacings are moved out of Groups and Nests and empty Groups and
--   Nests are removed.
-- - Now, all consecutive Spacings are ensured to be in the same list, so each
--   sequence of Spacings can be merged into a single one.
-- - Finally, Spacings right before a Nest should be moved inside in order to
--   get the right indentation.
fixup :: DocList -> DocList
fixup = map convertSpacing . moveLinesIn . mergeLines . concatMap moveLinesOut

moveLinesOut :: (Predoc []) -> DocList
moveLinesOut (Group xs) =
    let movedOut     = concatMap moveLinesOut xs
        (pre, rest)  = span isSpacing movedOut
        (post, body) = spanEnd isSpacing rest
    in case body of
            [] -> pre ++ post
            _  -> pre ++ (Group body : post)

moveLinesOut (Nest level xs) =
    let movedOut     = concatMap moveLinesOut xs
        (pre, rest)  = span isSpacing movedOut
        (post, body) = spanEnd isSpacing rest
    in case body of
            [] -> pre ++ post
            _  -> pre ++ (Nest level body : post)

moveLinesOut x = [x]

mergeLines :: DocList -> DocList
mergeLines []                           = []
mergeLines (Spacing Break : Spacing Softspace : xs)
    = mergeLines $ Spacing Space : xs
mergeLines (Spacing Softspace : Spacing Break : xs)
    = mergeLines $ Spacing Space : xs
mergeLines (Spacing a : Spacing b : xs) = mergeLines $ Spacing (max a b) : xs
mergeLines (Text a : Text b : xs)       = mergeLines $ Text (a <> b) : xs
mergeLines (Group xs : ys)              = Group (mergeLines xs) : mergeLines ys
mergeLines (Nest n xs : ys)             = Nest n (mergeLines xs) : mergeLines ys
mergeLines (x : xs)                     = x : mergeLines xs

moveLinesIn :: DocList -> DocList
moveLinesIn [] = []
moveLinesIn (Spacing l : Nest level xs : ys) =
    Nest level (Spacing l : moveLinesIn xs) : moveLinesIn ys

moveLinesIn (Nest level xs : ys) =
    Nest level (moveLinesIn xs) : moveLinesIn ys

moveLinesIn (Group xs : ys) =
    Group (moveLinesIn xs) : moveLinesIn ys

moveLinesIn (x : xs) = x : moveLinesIn xs

convertSpacing :: Predoc [] -> Predoc []
convertSpacing (Group xs)          = Group (map convertSpacing xs)
convertSpacing (Nest n xs)         = Nest n (map convertSpacing xs)
convertSpacing (Spacing Softbreak) = Group [Spacing Break]
convertSpacing (Spacing Softspace) = Group [Spacing Space]
convertSpacing (Spacing Hardspace) = Text " "
convertSpacing x                   = x

layout :: Pretty a => Int -> a -> Text
layout w = layoutGreedy w . fixup . flatten . pretty

-- 1. Flatten Docs to DocLists.
-- 2. Move and merge Spacings.
-- 3. Convert Softlines to Grouped Lines and Hardspaces to Texts.
-- 4. For each Text or Group, try to fit as much as possible on a line
-- 5. For each Group, if it fits on a single line, render it that way.
-- 6. If not, convert lines to hardlines and unwrap the group

-- | To support i18n, this function needs to be patched.
textWidth :: Text -> Int
textWidth = Text.length

fits :: Int -> DocList -> Maybe Text
fits c _ | c < 0 = Nothing
fits _ [] = Just ""
fits c (x:xs) = case x of
    Text t            -> (t<>) <$> fits (c - textWidth t) xs
    Spacing Softbreak -> fits c xs
    Spacing Break     -> fits c xs
    Spacing Softspace -> (" "<>) <$> fits (c - 1) xs
    Spacing Space     -> (" "<>) <$> fits (c - 1) xs
    Spacing Hardspace -> (" "<>) <$> fits (c - 1) xs
    Spacing Hardline  -> Nothing
    Spacing Emptyline -> Nothing
    Newline           -> Nothing
    Group ys          -> fits c $ ys ++ xs
    Nest _ ys         -> fits c $ ys ++ xs

firstLineWidth :: DocList -> Int
firstLineWidth []                       = 0
firstLineWidth (Text t : xs)            = textWidth t + firstLineWidth xs
firstLineWidth (Spacing Hardspace : xs) = 1 + firstLineWidth xs
firstLineWidth (Spacing _ : _)          = 0
firstLineWidth (Newline : _)            = 0
firstLineWidth (Nest _ xs : ys)         = firstLineWidth (xs ++ ys)
firstLineWidth (Group xs : ys)          = firstLineWidth (xs ++ ys)

data Chunk = Chunk Int (Predoc [])

indent :: Int -> Text
indent n = "\n" <> Text.replicate n " "

unChunk :: Chunk -> Predoc []
unChunk (Chunk _ doc) = doc

layoutGreedy :: Int -> DocList -> Text
layoutGreedy w doc = Text.concat $ go 0 [Chunk 0 $ Group doc]
    where go _ [] = []
          go c (Chunk i x : xs) = case x of
            Text t            -> t   : go (c + textWidth t) xs

            Spacing Softbreak -> indent i  : go i xs
            Spacing Break     -> indent i  : go i xs
            Spacing Softspace -> indent i  : go i xs
            Spacing Space     -> indent i  : go i xs
            Spacing Hardspace -> " "       : go (c + 1) xs
            Spacing Hardline  -> indent i  : go i xs
            Spacing Emptyline -> "\n" : indent i : go i xs

            Newline           -> case xs of
                []                    -> ["\n"]
                (Chunk _ Newline : _) -> "\n" : go i xs
                _                     -> indent i : go i xs

            Nest l ys         -> go c $ map (Chunk (i + l)) ys ++ xs
            Group ys          ->
                case fits (w - c - firstLineWidth (map unChunk xs)) ys of
                     Nothing  -> go c $ map (Chunk i) ys ++ xs
                     Just t   -> t : go (c + textWidth t) xs
