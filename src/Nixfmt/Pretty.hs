{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes, TupleSections #-}

module Nixfmt.Pretty where

import Prelude hiding (String)

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text, isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Text as Text
  (dropEnd, empty, init, isInfixOf, last, null, strip, takeWhile)

-- import Debug.Trace (traceShowId)
import Nixfmt.Predoc
  (Doc, Pretty, base, emptyline, group, group', hardline, hardspace, hcat, line, line',
  nest, newline, pretty, sepBy, softline, softline', text, textWidth)
import Nixfmt.Types
  (Ann(..), Binder(..), Expression(..), Item(..), Items(..), Leaf,
  ParamAttr(..), Parameter(..), Selector(..), SimpleSelector(..),
  StringPart(..), Term(..), Token(..), TrailingComment(..), Trivia, Trivium(..),
  Whole(..), tokenText, mapFirstToken')
import Nixfmt.Util (commonIndentation, isSpaces, replaceMultiple)
import GHC.Stack (HasCallStack)

prettyCommentLine :: Text -> Doc
prettyCommentLine l
    | Text.null l = emptyline
    | otherwise   = text l <> hardline

toLineComment :: TrailingComment -> Trivium
toLineComment (TrailingComment c) = LineComment $ " " <> c

-- The prime variant also strips leading * prefix
toLineComment' :: Text -> Trivium
toLineComment' c = LineComment $ fromMaybe (" " <> c) $ stripPrefix "*" c

-- If the token has some trailing comment after it, move that in front of the token
moveTrailingCommentUp :: Ann a -> Ann a
moveTrailingCommentUp (Ann pre a (Just post)) = Ann (pre ++ [toLineComment post]) a Nothing
moveTrailingCommentUp a = a

-- Make sure a group is not expanded because the token that starts it has
-- leading comments. This will render both arguments as a group, but
-- if the first argument has some leading comments they will be put before
-- the group
groupWithStart :: HasCallStack => Pretty a => Ann a -> Doc -> Doc
groupWithStart (Ann leading a trailing) b
    = pretty leading <> group (pretty a <> pretty trailing <> b)

instance Pretty TrailingComment where
    pretty (TrailingComment c)
        = hardspace <> text "#" <> hardspace <> text c <> hardline

instance Pretty Trivium where
    pretty EmptyLine        = emptyline
    pretty (LineComment c)  = text "#" <> pretty c <> hardline
    pretty (BlockComment c)
        | all ("*" `isPrefixOf`) (tail c) = hcat (map toLineComment' c)
        | otherwise
            = base $ text "/*" <> hardspace
              <> nest 3 (hcat (map prettyCommentLine c))
              <> text "*/" <> hardline

instance Pretty a => Pretty (Item a) where
    pretty (DetachedComments trivia) = pretty trivia
    pretty (CommentedItem trivia x) = pretty trivia <> group x

-- For lists, attribute sets and let bindings
prettyItems :: Pretty a => Doc -> Items a -> Doc
prettyItems sep = prettyItems' . unItems
  where
    prettyItems' :: Pretty a => [Item a] -> Doc
    prettyItems' [] = mempty
    prettyItems' [item] = pretty item
    prettyItems' (item : xs)
        = pretty item
        <> case item of { CommentedItem _ _ -> sep; DetachedComments _ -> emptyline }
        <> prettyItems' xs

instance Pretty [Trivium] where
    pretty []     = mempty
    pretty trivia = hardline <> hcat trivia

instance Pretty a => Pretty (Ann a) where
    pretty (Ann leading x trailing)
        = pretty leading <> pretty x <> pretty trailing

instance Pretty SimpleSelector where
    pretty (IDSelector i)              = pretty i
    pretty (InterpolSelector interpol) = pretty interpol
    pretty (StringSelector (Ann leading s trailing))
        = pretty leading <> prettySimpleString s <> pretty trailing

instance Pretty Selector where
    pretty (Selector dot sel Nothing)
        = pretty dot <> pretty sel

    pretty (Selector dot sel (Just (kw, def)))
        = base $ pretty dot <> pretty sel
          <> softline <> nest 2 (pretty kw <> hardspace <> pretty def)

-- in attrsets and let bindings
instance Pretty Binder where
    -- `inherit bar` statement
    pretty (Inherit inherit Nothing ids semicolon)
        = base $ group (pretty inherit
            <> (if null ids then mempty else line <> nest 2 (sepBy (if length ids < 4 then line else hardline) ids) <> line')
            <> pretty semicolon)

    -- `inherit (foo) bar` statement
    pretty (Inherit inherit (Just source) ids semicolon)
        = base $ group (pretty inherit <> nest 2 (
            (group' False (line <> pretty source))
                <> if null ids then mempty else line
                <> sepBy (if length ids < 4 then line else hardline) ids
        ) <> line' <> pretty semicolon)

    -- `foo = bar`
    pretty (Assignment selectors assign expr semicolon)
        = base $ group $ hcat selectors
                 <> nest 2 (hardspace <> pretty assign <> inner) <> pretty semicolon
        where
          inner =
            case expr of
              -- Absorbable term. Always start on the same line, keep semicolon attatched
              (Term t) | isAbsorbable t -> hardspace <> group expr
              -- Not all strings are absorbably, but in this case we always want to keep them attached.
              -- Because there's nothing to gain from having them start on a new line.
              (Term (String _)) -> hardspace <> group expr
              -- Same for path
              (Term (Path _)) -> hardspace <> group expr
              -- Non-absorbable term
              -- If it is multi-line, force it to start on a new line with indentation
              (Term _) -> group' False (line <> pretty expr)
              -- Function call
              -- Absorb if all arguments except the last fit into the line, start on new line otherwise
              (Application f a) -> group $ prettyApp hardline line line' mempty f a
              -- With expression with absorbable body: Try to absorb and keep the semicolon attached, spread otherwise
              (With _ _ _ (Term t)) | isAbsorbable t -> softline <> group' False (pretty expr <> softline')
              -- Special case `//` operator to treat like an absorbable term
              (Operation _ (Ann _ TUpdate _) _) -> softline <> group' False (pretty expr <> softline')
              -- Everything else:
              -- If it fits on one line, it fits
              -- If it fits on one line but with a newline after the `=`, it fits (including semicolon)
              -- Otherwise, start on new line, expand fully (including the semicolon)
              _ -> line <> group' False (pretty expr <> line')

-- | Pretty print a term without wrapping it in a group.
prettyTerm :: Term -> Doc
prettyTerm (Token t) = pretty t
prettyTerm (String s) = pretty s
prettyTerm (Path p) = pretty p
-- Selection (`foo.bar.baz`) case distinction on the first element (`foo`):
-- If it is an ident, keep it all together
prettyTerm (Selection term@(Token _) selectors) = pretty term <> hcat selectors
-- If it is a parenthesized expression, maybe add a line break
prettyTerm (Selection term@(Parenthesized _ _ _) selectors) = pretty term <> softline' <> hcat selectors
-- Otherwise, very likely add a line break
prettyTerm (Selection term selectors) = pretty term <> line' <> hcat selectors

-- Empty list
prettyTerm (List (Ann leading paropen Nothing) (Items []) (Ann [] parclose trailing))
    = pretty leading <> pretty paropen <> hardspace <> pretty parclose <> pretty trailing

-- Singleton list
-- Expand unless absorbable term or single line
prettyTerm (List paropen@(Ann _ _ Nothing) (Items [item@(CommentedItem iComment item')]) parclose@(Ann [] _ _))
        = base $ groupWithStart paropen $
            (if isAbsorbable item' && null iComment then
                (hardspace <> pretty item' <> hardspace)
            else
                (line <> nest 2 (pretty item) <> line)
            )
            <> pretty parclose

-- General list (len >= 2)
-- Always expand
prettyTerm (List (Ann pre paropen post) items parclose) =
    base $ pretty (Ann pre paropen Nothing) <> hardline
    <> nest 2 ((pretty post) <> prettyItems hardline items) <> hardline
    <> pretty parclose

-- Empty, non-recursive attribute set
prettyTerm (Set Nothing (Ann [] paropen Nothing) (Items []) parclose@(Ann [] _ _))
    = pretty paropen <> hardspace <> pretty parclose

-- General set
-- Singleton sets are allowed to fit onto one line,
-- but apart from that always expand.
prettyTerm (Set krec (Ann pre paropen post) binders parclose)
    = base $ pretty (fmap (, hardspace) krec) <>
        pretty (Ann pre paropen Nothing) <> line
        <> nest 2 (pretty post <> prettyItems hardline binders) <> line
        <> pretty parclose

-- Parenthesized application
prettyTerm (Parenthesized (Ann pre paropen post) (Application f a) parclose)
    = base $ groupWithStart (Ann pre paropen Nothing) $ nest 2 (
            -- Move comment trailing on '(' to next line, combine with comment from application
            case pretty post of { [] -> []; c -> hardline <> c }
            <> base (prettyApp hardline mempty line' hardline f a)
            <> case pretty post of  { [] -> mempty; _ -> hardline }
        ) <> pretty parclose

-- Parentheses
prettyTerm (Parenthesized paropen expr parclose)
    = base $ groupWithStart paropen (lineL <> nest 2 (group expr) <> lineR <> pretty parclose)
  where
    (lineL, lineR) =
      case expr of
        -- Start on the same line for these
        (Term t) | isAbsorbable t -> (mempty, mempty)
        -- unreachable
        (Application _ _) -> (mempty, mempty)
        -- Absorb function declarations but only those with simple parameter(s)
        (Abstraction _ _ _) | isAbstractionWithAbsorbableTerm expr -> (mempty, mempty)
        (Operation _ _ _) -> (line', line')
        -- Same thing for selections
        (Term (Selection t _)) | isAbsorbable t -> (line', line')
        (Term (Selection _ _)) -> (mempty, line')
        -- Start on a new line for the others
        _ -> (line', line')
    isAbstractionWithAbsorbableTerm (Abstraction (IDParameter _) _ (Term t)) | isAbsorbable t = True
    isAbstractionWithAbsorbableTerm (Abstraction (IDParameter _) _ body) = isAbstractionWithAbsorbableTerm body
    isAbstractionWithAbsorbableTerm _ = False

instance Pretty Term where
    pretty l@List{} = group $ prettyTerm l
    pretty x        = prettyTerm x

instance Pretty ParamAttr where
    -- Simple parameter, move comment around
    -- Move comments around when switching from leading comma to trailing comma style:
    -- `, name # foo` → `name, #foo`
    pretty (ParamAttr (Ann trivia name (Just comment)) Nothing (Just (Ann trivia' comma Nothing)))
            = pretty (ParamAttr (Ann trivia name Nothing) Nothing (Just (Ann trivia' comma (Just comment))))

    -- Simple parameter, move comment around and add trailing comma
    -- Same as above, but also add trailing comma
    pretty (ParamAttr (Ann trivia name (Just comment)) Nothing Nothing)
            = pretty (ParamAttr (Ann trivia name Nothing) Nothing (Just (Ann [] TComma (Just comment))))

    -- Simple parameter
    -- Still need to handle missing trailing comma here, because the special cases above are not exhaustive
    pretty (ParamAttr name Nothing maybeComma)
        = pretty name <> (fromMaybe (text ",") (fmap pretty maybeComma))

    -- With ? default
    pretty (ParamAttr name (Just (qmark, def)) maybeComma)
        = group (pretty name <> hardspace <> pretty qmark
            <> absorb softline mempty (Just 2) def)
            <> (fromMaybe (text ",") (fmap pretty maybeComma))

    -- `...`
    pretty (ParamEllipsis ellipsis)
        = pretty ellipsis

-- When a `, name` entry has some line comments before it, they are actually attached to the comment
-- of the preceding item. Move them to the next one
moveParamComments :: [ParamAttr] -> [ParamAttr]
moveParamComments
    ((ParamAttr name maybeDefault (Just (Ann trivia comma Nothing))) : (ParamAttr (Ann [] name' Nothing) maybeDefault' maybeComma') : xs)
    = (ParamAttr name maybeDefault (Just (Ann [] comma Nothing))) : moveParamComments ((ParamAttr (Ann trivia name' Nothing) maybeDefault' maybeComma') : xs)
moveParamComments (x : xs) = x : moveParamComments xs
moveParamComments [] = []

instance Pretty Parameter where
    -- param:
    pretty (IDParameter i) = pretty i

    -- {}:
    pretty (SetParameter bopen [] bclose)
        = group $ pretty bopen <> hardspace <> pretty bclose

    -- { stuff }:
    pretty (SetParameter bopen attrs bclose)
        = groupWithStart bopen $ hardline
                  <> nest 2 (((sepBy hardline) . moveParamComments) attrs) <> hardline
                  <> pretty bclose

    pretty (ContextParameter param1 at param2)
        = pretty param1 <> pretty at <> pretty param2


-- Function application
-- Some example mapping of Nix code to Doc (using brackets as groups, but omitting the outermost group
-- and groups around the expressions for conciseness):
-- `f a` -> pre f line a post
-- `f g a` -> pre [f line g] line a post
-- `f g h a` -> pre [[f line g] line h] line a post
-- `f g h i a` -> pre [[[f line g] line h] line i] line a post
-- As you can see, it separates the elements by `line` whitespace. However, there are three tricks to make it look good:
-- First, for each function call (imagine the fully parenthesised Nix code), we group it. Due to the greedy expansion
-- of groups this means that it will place as many function calls on the first line as possible, but then all the remaining
-- ones on a separate line each.
-- Second, the last argument is declared as "priority" group, meaning that the layouting algorithm will try to expand
-- it first when things do not fit onto one line. This allows the last argument to be multi-line without forcing the
-- preceding arguments to be multiline.
-- Third, callers may inject `pre` and `post` tokens (mostly newlines) into the inside of the group.
-- This means that callers can say "try to be compact first, but if more than the last argument does not fit onto the line,
-- then start on a new line instead".
-- Out of necessity, callers may also inject `commentPre` and `commentPost`, which will be added before/after the entire
-- thing if the function has a comment associated with its first token
prettyApp :: Doc -> Doc -> Doc -> Doc -> Expression -> Expression -> Doc
prettyApp commentPre pre post commentPost f a
    = let
        absorbApp (Application f' a') = (group $ absorbApp f') <> line <> (nest 2 (group a'))
        absorbApp expr = pretty expr

        absorbLast (Term t) | isAbsorbable t
            = group' True $ nest 2 $ prettyTerm t
        absorbLast (Term (Parenthesized (Ann pre' open post') expr close))
            = group' True $ nest 2 $ base $ pretty (Ann pre' open Nothing) <> line'
                <> group (nest 2 (pretty post' <> pretty expr))
                <> line' <> pretty close
        absorbLast arg = group' True $ nest 2 $ pretty arg

        -- Extract comment before the first function and move it out, to prevent functions being force-expanded
        (fWithoutComment, comment) = mapFirstToken' (\(Ann leading token trailing) -> (Ann [] token trailing, leading)) f
        in
        (if null comment then mempty else commentPre)
        <> pretty comment <> (group' False $
            pre <> group (absorbApp fWithoutComment) <> line <> absorbLast a <> post)
        <> (if null comment then mempty else commentPost)

isAbsorbable :: Term -> Bool
isAbsorbable (String (Ann _ parts@(_:_:_) _))
    = not $ isSimpleString parts
isAbsorbable (Set _ _ (Items (_:_)) _)                                   = True
isAbsorbable (List (Ann [] _ Nothing) (Items [CommentedItem [] _]) _)    = True
isAbsorbable (Parenthesized (Ann [] _ Nothing) (Term t) _)               = isAbsorbable t
isAbsorbable (List _ (Items (_:_:_)) _)                                  = True
isAbsorbable _                                                           = False

absorb :: Doc -> Doc -> Maybe Int -> Expression -> Doc
absorb left right _ (Term t)
    | isAbsorbable t = toHardspace left <> prettyTerm t <> toHardspace right
    where toHardspace x | x == mempty    = mempty
                        | x == softline' = mempty
                        | x == line'     = mempty
                        | otherwise      = hardspace

absorb left right Nothing x = left <> pretty x <> right
absorb left right (Just level) x
    = left <> nest level (pretty x) <> right

absorbSet :: Expression -> Doc
absorbSet = absorb line mempty Nothing

-- Only absorb "else if"
absorbElse :: Expression -> Doc
absorbElse (If if_ cond then_ expr0 else_ expr1)
    -- `if cond then` if it fits on one line, otherwise `if\n  cond\nthen` (with cond indented)
    -- Using hardline here is okay because it will only apply to nested ifs, which should not be inline anyways.
    = hardspace <> (group (pretty if_ <> line <> nest 2 (pretty cond) <> line <> pretty then_))
      <> hardline <> nest 2 (group expr0) <> hardline
      <> pretty else_ <> absorbElse expr1
absorbElse x
    = line <> nest 2 (group x)

instance Pretty Expression where
    pretty (Term t) = pretty t

    pretty (With with expr0 semicolon expr1)
        = base (pretty with <> hardspace
          <> nest 2 (group expr0) <> pretty semicolon)
          <> absorbSet expr1

    -- Let bindings are always fully expanded (no single-line form)
    -- We also take the comments around the `in` (trailing, leading and detached binder comments)
    -- and move them down to the first token of the body
    pretty (Let let_ binders (Ann leading in_ trailing) expr)
        = base $ letPart <> hardline <> inPart
        where
          -- Convert the TrailingComment to a Trivium, if present
          convertTrailing Nothing = []
          convertTrailing (Just (TrailingComment t)) = [(LineComment (" " <> t))]

          -- Extract detached comments at the bottom.
          -- This uses a custom variant of span/spanJust/spanMaybe.
          -- Note that this is a foldr which walks from the bottom, but the lists
          -- are constructed in a way that they end up correct again.
          (binderComments, bindersWithoutComments)
            = foldr
                (\item -> \(start, rest) ->
                    case item of
                        (DetachedComments inner) | null rest -> (inner : start, rest)
                        _ -> (start, item : rest)
                )
                ([], [])
                (unItems binders)

          letPart = groupWithStart let_ $ hardline <> letBody
          letBody = nest 2 $ prettyItems hardline (Items bindersWithoutComments)
          inPart = groupWithStart (Ann [] in_ Nothing) $ hardline
              -- Take our trailing and inject it between `in` and body
              <> pretty (concat binderComments ++ leading ++ convertTrailing trailing)
              <> pretty expr <> hardline

    pretty (Assert assert cond semicolon expr)
        = base (pretty assert <> hardspace
          <> nest 2 (group cond) <> pretty semicolon)
          <> absorbSet expr

    pretty (If if_ cond then_ expr0 else_ expr1)
        = base $ group $
            -- `if cond then` if it fits on one line, otherwise `if\n  cond\nthen` (with cond indented)
            (groupWithStart if_ (line <> nest 2 (pretty cond) <> line <> pretty then_))
            <> line <> nest 2 (group expr0) <> line
            <> pretty else_ <> absorbElse expr1

    pretty (Abstraction (IDParameter param) colon body)
        = pretty param <> pretty colon <> absorbAbs body
        where absorbAbs (Abstraction (IDParameter param0) colon0 body0) =
                  hardspace <> pretty param0 <> pretty colon0 <> absorbAbs body0
              absorbAbs x = absorbSet x

    pretty (Abstraction param colon body)
        = pretty param <> pretty colon <> line <> pretty body

    pretty (Application f a)
        = prettyApp mempty mempty mempty mempty f a

    -- '//' operator
    pretty (Operation a op@(Ann _ TUpdate _) b)
        = pretty a <> softline <> pretty op <> hardspace <> pretty b
    -- binary operators
    pretty (Operation a op@(Ann _ op' _) b)
        | op' == TLess || op' == TGreater || op' == TLessEqual || op' == TGreaterEqual || op' == TEqual || op' == TUnequal
        = pretty a <> softline <> pretty op <> hardspace <> pretty b
    -- all other operators
    pretty operation@(Operation _ op _)
        = let
            -- Walk the operation tree and put a list of things on the same level.
            -- We still need to keep the operators around because they might have comments attached to them.
            -- An operator is put together with its succeeding expression. Only the first operand has none.
            flatten :: Maybe Leaf -> Expression -> [(Maybe Leaf, Expression)]
            flatten opL (Operation a opR b) | opR == op = (flatten opL a) ++ (flatten (Just opR) b)
            flatten opL x = [(opL, x)]

            -- Called on every operand except the first one (a.k.a. RHS)
            absorbOperation :: Expression -> Doc
            absorbOperation (Term t) | isAbsorbable t = hardspace <> (base $ pretty t)
            -- Force nested operations to start on a new line
            absorbOperation x@(Operation _ _ _) = group' False $ line <> pretty x
            -- Force applications to start on a new line if more than the last argument is multiline
            absorbOperation (Application f a) = group $ hardspace <> prettyApp hardline line mempty mempty f a
            absorbOperation x = hardspace <> pretty x

            prettyOperation :: (Maybe Leaf, Expression) -> Doc
            -- First element
            prettyOperation (Nothing, expr) = pretty expr
            -- The others
            prettyOperation ((Just op'), expr) =
                line <> pretty (moveTrailingCommentUp op') <> nest 2 (absorbOperation expr)

            -- Extract comment before the first operand and move it out, to prevent force-expanding the expression
            (operationWithoutComment, comment) = mapFirstToken' (\(Ann leading token trailing) -> (Ann [] token trailing, leading)) operation
          in
            pretty comment <> (group $
                (concat . map prettyOperation . (flatten Nothing)) operationWithoutComment)

    pretty (MemberCheck expr qmark sel)
        = pretty expr <> softline
          <> pretty qmark <> hardspace <> hcat sel

    pretty (Negation minus expr)
        = pretty minus <> pretty expr

    pretty (Inversion bang expr)
        = pretty bang <> pretty expr

instance Pretty a => Pretty (Whole a) where
    pretty (Whole x finalTrivia)
        = group $ pretty x <> pretty finalTrivia

instance Pretty Token where
    pretty = text . tokenText

-- STRINGS

isSimpleSelector :: Selector -> Bool
isSimpleSelector (Selector _ (IDSelector _) Nothing) = True
isSimpleSelector _                                   = False

isSimple :: Expression -> Bool
isSimple (Term (Token (Ann [] (Identifier _) Nothing))) = True
isSimple (Term (Selection t selectors))
    = isSimple (Term t) && all isSimpleSelector selectors
isSimple _ = False

hasQuotes :: [StringPart] -> Bool
hasQuotes []                = False
hasQuotes (TextPart x : xs) = Text.isInfixOf "\"" x || hasQuotes xs
hasQuotes (_ : xs)          = hasQuotes xs

hasDualQuotes :: [StringPart] -> Bool
hasDualQuotes []                = False
hasDualQuotes (TextPart x : xs) = Text.isInfixOf "''" x || hasDualQuotes xs
hasDualQuotes (_ : xs)          = hasDualQuotes xs

endsInSingleQuote :: [StringPart] -> Bool
endsInSingleQuote []           = False
endsInSingleQuote xs =
    case last xs of
         (TextPart x) -> x /= Text.empty && Text.last x == '\''
         _            -> False

isIndented :: [[StringPart]] -> Bool
isIndented parts =
    case commonIndentation inits of
         Just "" -> False
         _       -> True
    where textInit (TextPart t : xs) = t <> textInit xs
          textInit _                 = ""
          nonEmpty (TextPart "" : xs) = nonEmpty xs
          nonEmpty []                 = False
          nonEmpty _                  = True
          inits = map textInit $ filter nonEmpty parts

-- | If the last line has at least one space but nothing else, it cannot be
-- cleanly represented in an indented string.
lastLineIsSpaces :: [[StringPart]] -> Bool
lastLineIsSpaces [] = False
lastLineIsSpaces xs = case last xs of
    [TextPart t] -> isSpaces t
    _            -> False

isInvisibleLine :: [StringPart] -> Bool
isInvisibleLine []           = True
isInvisibleLine [TextPart t] = Text.null $ Text.strip t
isInvisibleLine _            = False

isSimpleString :: [[StringPart]] -> Bool
isSimpleString [parts]
    | hasDualQuotes parts       = True
    | endsInSingleQuote parts   = True
    | isIndented [parts]        = True
    | hasQuotes parts           = False
    | otherwise                 = True

isSimpleString parts
    | all isInvisibleLine parts = True
    | isIndented parts          = True
    | lastLineIsSpaces parts    = True
    | otherwise                 = False

instance Pretty StringPart where
    pretty (TextPart t) = text t
    pretty (Interpolation (Whole (Term t) []))
        | isAbsorbable t
            = group $ text "${" <> prettyTerm t <> text "}"

    pretty (Interpolation (Whole expr []))
        | isSimple expr
            = text "${" <> pretty expr <> text "}"

    pretty (Interpolation whole)
        = group $ text "${" <> line'
            <> nest 2 (pretty whole) <> line'
            <> text "}"

instance Pretty [StringPart] where
    pretty [Interpolation expr]
        = group $ text "${" <> pretty expr <> text "}"

    -- If we split a string line over multiple code lines due to large
    -- interpolations, make sure to indent based on the indentation of the line
    -- in the string.
    pretty (TextPart t : parts)
        = text t <> nest indentation (hcat parts)
        where indentation = textWidth $ Text.takeWhile isSpace t

    pretty parts = hcat parts

instance Pretty [[StringPart]] where
    pretty parts
        | isSimpleString parts = prettySimpleString parts
        | otherwise            = prettyIndentedString parts

type UnescapeInterpol = Text -> Text
type EscapeText = Text -> Text

prettyLine :: EscapeText -> UnescapeInterpol -> [StringPart] -> Doc
prettyLine escapeText unescapeInterpol
    = pretty . unescapeInterpols . map escape
    where escape (TextPart t) = TextPart (escapeText t)
          escape x            = x

          unescapeInterpols [] = []
          unescapeInterpols (TextPart t : TextPart u : xs)
              = unescapeInterpols (TextPart (t <> u) : xs)
          unescapeInterpols (TextPart t : xs@(Interpolation{} : _))
              = TextPart (unescapeInterpol t) : unescapeInterpols xs
          unescapeInterpols (x : xs) = x : unescapeInterpols xs

prettySimpleString :: [[StringPart]] -> Doc
prettySimpleString parts = group $
    text "\""
    <> sepBy (text "\\n") (map (prettyLine escape unescapeInterpol) parts)
    <> text "\""
    where escape = replaceMultiple
              [ ("$\\${", "$${")
              , ("${",    "\\${")
              , ("\"",    "\\\"")
              , ("\r",    "\\r")
              , ("\\",    "\\\\")
              ]

          unescapeInterpol t
              | "$" `isSuffixOf` t = Text.init t <> "\\$"
              | otherwise          = t

prettyIndentedString :: [[StringPart]] -> Doc
prettyIndentedString parts = group $ base $
    text "''" <> line'
    <> nest 2 (sepBy newline (map (prettyLine escape unescapeInterpol) parts))
    <> text "''"
    where escape = replaceMultiple
              [ ("'${", "''\\'''${")
              , ("${", "''${")
              , ("''", "'''")
              ]

          unescapeInterpol t
              | Text.null t        = t
              | Text.last t /= '$' = t
              | trailingQuotes (Text.init t) `mod` 3 == 0
                  = Text.init t <> "''$"
              | trailingQuotes (Text.init t) `mod` 3 == 1
                  = Text.dropEnd 2 t <> "''\\'''$"
              | otherwise
                  = error "should never happen after escape"

          trailingQuotes t
              | "'" `isSuffixOf` t = 1 + trailingQuotes (Text.init t)
              | otherwise          = 0 :: Int
