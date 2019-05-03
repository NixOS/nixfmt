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
    , pretty'
    ) where

import Data.List hiding (group)
import Data.Text (Text, pack)
import qualified Data.Text.Prettyprint.Doc as PP

data Tree a
    = EmptyTree
    | Leaf a
    | Node (Tree a) (Tree a)
    deriving (Show, Functor)

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
    | Newline
    deriving (Show, Eq, Ord)

data Predoc f
    = Text Text
    | Spacing Spacing
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
newline = Leaf (Spacing Newline)

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
fixup = moveLinesIn . mergeLines . concatMap moveLinesOut

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
mergeLines (Spacing Newline : xs)       = Spacing Newline : mergeLines xs
mergeLines (Spacing x : Spacing Newline : xs)
    = Spacing x : Spacing Newline : mergeLines xs
mergeLines (Spacing Break : Spacing Softspace : xs)
    = mergeLines $ Spacing Space : xs
mergeLines (Spacing Softspace : Spacing Break : xs)
    = mergeLines $ Spacing Space : xs
mergeLines (Spacing a : Spacing b : xs) = mergeLines $ Spacing (max a b) : xs
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

pretty' :: Pretty a => a -> PP.Doc ann
pretty' = PP.pretty . fixup . flatten . pretty

instance PP.Pretty (Predoc []) where
    pretty (Text t)            = PP.pretty t

    pretty (Spacing Softbreak) = PP.softline'
    pretty (Spacing Break)     = PP.line'
    pretty (Spacing Softspace) = PP.softline
    pretty (Spacing Space)     = PP.line
    pretty (Spacing Hardspace) = PP.pretty (pack " ")
    pretty (Spacing Hardline)  = PP.hardline
    pretty (Spacing Emptyline) = PP.hardline <> PP.hardline
    pretty (Spacing Newline)   = PP.hardline

    pretty (Group docs)        = PP.group (PP.pretty docs)
    pretty (Nest level docs)   = PP.nest level (PP.pretty docs)

    prettyList = PP.hcat . map PP.pretty
