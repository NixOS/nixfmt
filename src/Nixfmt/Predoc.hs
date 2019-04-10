{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | This module implements a layer around the prettyprinter package, making it
-- easier to use.
module Nixfmt.Predoc
    ( text
    , hcat
    , trivia
    , group
    , nest
    , break
    , space
    , hardline
    , emptyline
    , Doc
    , Pretty
    , pretty
    , putDocW
    , fixup
    , flatten
    ) where

import           Data.Text                      (Text, pack)
import qualified Data.Text.Prettyprint.Doc      as PP
import qualified Data.Text.Prettyprint.Doc.Util as PPU
import           Prelude                        hiding (break)

data Tree a = EmptyTree
            | Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show, Functor)

-- | Sequential Lines are reduced to a single Line by taking the maximum. This
-- means that e.g. a Space followed by an Emptyline results in just an
-- Emptyline.
data Line = Break
          | Space
          | Hardline
          | Emptyline
          deriving (Show, Eq, Ord)

data Predoc f
    = Trivia Text
    | Text Text
    | Line Line
    -- | Group predoc indicates either all or none of the spaces in predoc
    -- and not in a subgroup should be converted to line breaks.
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

instance Semigroup (Tree a) where
    left <> right = Node left right

instance Monoid (Tree (Predoc Tree)) where
    mempty = EmptyTree

text :: Text -> Doc
text = Leaf . Text

trivia :: Text -> Doc
trivia = Leaf . Trivia

group :: Doc -> Doc
group = Leaf . Group

nest :: Int -> Doc -> Doc
nest level = Leaf . Nest level

break :: Doc
break = Leaf (Line Break)

space :: Doc
space = Leaf (Line Space)

hardline :: Doc
hardline = Leaf (Line Hardline)

emptyline :: Doc
emptyline = Leaf (Line Emptyline)

hcat :: [Doc] -> Doc
hcat = mconcat

flatten :: Doc -> DocList
flatten = go []
    where go xs (Node x y)           = go (go xs y) x
          go xs EmptyTree            = xs
          go xs (Leaf (Group tree))  = Group (go [] tree) : xs
          go xs (Leaf (Nest l tree)) = Nest l (go [] tree) : xs
          go xs (Leaf (Line line))   = Line line : xs
          go xs (Leaf (Text t))      = Text t : xs
          go xs (Leaf (Trivia t))    = Trivia t : xs

isLine :: Predoc f -> Bool
isLine (Line _) = True
isLine _        = False

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = fmap reverse . span p . reverse

-- | Lines before a Nest should be moved inside, breaks at the end of a nest
-- should be moved outside. Lines at the start or end of a group should be
-- moved outside.

moveLinesOut :: DocList -> DocList
moveLinesOut [] = []
moveLinesOut (Group xs : ys) =
    let movedOut     = moveLinesOut xs
        (pre, rest)  = span isLine movedOut
        (post, body) = spanEnd isLine rest
    in pre ++ (Group body : (post ++ moveLinesOut ys))

moveLinesOut (Nest level xs : ys) =
    let movedOut     = moveLinesOut xs
        (pre, rest)  = span isLine movedOut
        (post, body) = spanEnd isLine rest
    in pre ++ (Nest level body : (post ++ moveLinesOut ys))

moveLinesOut (x : xs) = x : moveLinesOut xs

mergeLines :: DocList -> DocList
mergeLines []                     = []
mergeLines (Line a : Line b : xs) = Line (max a b) : mergeLines xs
mergeLines (Group xs : ys)        = Group (mergeLines xs) : mergeLines ys
mergeLines (Nest n xs : ys)       = Nest n (mergeLines xs) : mergeLines ys
mergeLines (x : xs)               = x : mergeLines xs

moveLinesIn :: DocList -> DocList
moveLinesIn [] = []
moveLinesIn (Line l : Nest level xs : ys) =
    Nest level (Line l : moveLinesIn xs) : moveLinesIn ys

moveLinesIn (Nest level xs : ys) =
    Nest level (moveLinesIn xs) : moveLinesIn ys

moveLinesIn (Group xs : ys) =
    Group (moveLinesIn xs) : moveLinesIn ys

moveLinesIn (x : xs) = x : moveLinesIn xs

fixup :: DocList -> DocList
fixup = moveLinesIn . mergeLines . moveLinesOut

putDocW :: Int -> Doc -> IO ()
putDocW n = PPU.putDocW n . PP.pretty

instance PP.Pretty Doc where
    pretty = PP.pretty . fixup . flatten

instance PP.Pretty (Predoc []) where
    pretty (Trivia t)        = PP.pretty t
    pretty (Text t)          = PP.pretty t
    pretty (Line Break)      = PP.line'
    pretty (Line Space)      = PP.line
    pretty (Line Hardline)   = PP.hardline
    pretty (Line Emptyline)  = PP.hardline <> PP.hardline
    pretty (Group docs)      = PP.group (PP.pretty docs)
    pretty (Nest level docs) = PP.nest level (PP.pretty docs)

    prettyList = PP.hcat . map PP.pretty
