{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes, TupleSections, LambdaCase #-}

module Nixfmt.Pretty where

import Prelude hiding (String)

import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.Text (Text, isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Text as Text
  (dropEnd, empty, init, isInfixOf, last, null, strip, takeWhile, all)

-- import Debug.Trace (traceShowId)
import Nixfmt.Predoc
  (Doc, Pretty, base, emptyline, group, group', hardline, hardspace, hcat, line, line',
  nest, newline, pretty, sepBy, surroundWith, softline, softline', text, comment, trailing, textWidth,
  unexpandSpacing')
import Nixfmt.Types
  (Ann(..), Binder(..), Expression(..), Item(..), Items(..), Leaf,
  ParamAttr(..), Parameter(..), Selector(..), SimpleSelector(..),
  StringPart(..), Term(..), Token(..), TrailingComment(..), Trivium(..),
  Whole(..), tokenText, mapFirstToken', mapLastToken')
import Nixfmt.Util (commonIndentation, isSpaces, replaceMultiple)

prettyCommentLine :: Text -> Doc
prettyCommentLine l
    | Text.null l = emptyline
    | otherwise   = comment l <> hardline

toLineComment :: TrailingComment -> Trivium
toLineComment (TrailingComment c) = LineComment $ " " <> c

-- The prime variant also strips leading * prefix
toLineComment' :: Text -> Trivium
toLineComment' c = LineComment $ fromMaybe (" " <> c) $ stripPrefix "*" c

-- If the token has some trailing comment after it, move that in front of the token
moveTrailingCommentUp :: Ann a -> Ann a
moveTrailingCommentUp (Ann pre a (Just post)) = Ann (pre ++ [toLineComment post]) a Nothing
moveTrailingCommentUp a = a

instance Pretty TrailingComment where
    pretty (TrailingComment c)
        = hardspace <> comment ("# " <> c) <> hardline

instance Pretty Trivium where
    pretty EmptyLine        = emptyline
    pretty (LineComment c)  = comment ("#" <> c) <> hardline
    pretty (BlockComment c)
        | all ("*" `isPrefixOf`) (tail c) = hcat (map toLineComment' c)
        | otherwise
            = base $ comment "/*" <> hardspace
              <> nest 3 (hcat (map prettyCommentLine c))
              <> comment "*/" <> hardline

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
    pretty (Ann leading x trailing')
        = pretty leading <> pretty x <> pretty trailing'

instance Pretty SimpleSelector where
    pretty (IDSelector i)              = pretty i
    pretty (InterpolSelector interpol) = pretty interpol
    pretty (StringSelector (Ann leading s trailing'))
        = pretty leading <> prettySimpleString s <> pretty trailing'

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
        = base $ group $ pretty inherit
            <> (if null ids then pretty semicolon else
                line <> nest 2 (sepBy (if length ids < 4 then line else hardline) ids <> line' <> pretty semicolon)
            )

    -- `inherit (foo) bar` statement
    pretty (Inherit inherit (Just source) ids semicolon)
        = base $ group $ pretty inherit <> nest 2 (
            (group' False (line <> pretty source))
            <> if null ids then pretty semicolon else line
            <> sepBy (if length ids < 4 then line else hardline) ids
            <> line' <> pretty semicolon
        )

    -- `foo = bar`
    pretty (Assignment selectors assign expr semicolon)
        = base $ group $ hcat selectors
                 <> nest 2 (hardspace <> pretty assign <> inner) <> pretty semicolon
        where
        inner =
            case expr of
              -- Absorbable term. Always start on the same line, keep semicolon attatched
              (Term t) | isAbsorbable t -> hardspace <> prettyTermWide t
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
              (Application f a) -> prettyApp hardline line mempty mempty f a
              -- Absorb function declarations but only those with simple parameter(s)
              (Abstraction _ _ _) | isAbstractionWithAbsorbableTerm expr -> hardspace <> group expr
              -- With expression with absorbable body: Try to absorb and keep the semicolon attached, spread otherwise
              (With _ _ _ (Term t)) | isAbsorbable t -> softline <> group expr
              -- Special case `//` operations to be more compact in some cases
              -- Case 1: two arguments, LHS is absorbable term, RHS fits onto the last line
              (Operation (Term t) (Ann [] TUpdate Nothing) b) | isAbsorbable t ->
                group' False $ line <> group' True (prettyTermWide t) <> line <> pretty TUpdate <> hardspace <> pretty b
              -- Case 2a: LHS fits onto first line, RHS is an absorbable term
              (Operation l (Ann [] TUpdate Nothing) (Term t)) | isAbsorbable t ->
                group' False $ line <> pretty l <> line <> group' True (pretty TUpdate <> hardspace <> prettyTermWide t)
              -- Case 2b: LHS fits onto first line, RHS is a function application
              (Operation l (Ann [] TUpdate Nothing) (Application f a)) ->
                line <> (group l) <> line <> prettyApp hardline (pretty TUpdate <> hardspace) mempty mempty f a
              -- Special case `++` operations to be more compact in some cases
              -- Case 1: two arguments, LHS is absorbable term, RHS fits onto the last line
              (Operation (Term t) (Ann [] TConcat Nothing) b) | isAbsorbable t ->
                group' False $ line <> group' True (prettyTermWide t) <> line <> pretty TConcat <> hardspace <> pretty b
              -- Case 2a: LHS fits onto first line, RHS is an absorbable term
              (Operation l (Ann [] TConcat Nothing) (Term t)) | isAbsorbable t ->
                group' False $ line <> pretty l <> line <> group' True (pretty TConcat <> hardspace <> prettyTermWide t)
              -- Case 2b: LHS fits onto first line, RHS is a function application
              (Operation l (Ann [] TConcat Nothing) (Application f a)) ->
                line <> (group l) <> line <> prettyApp hardline (pretty TConcat <> hardspace) mempty mempty f a
              -- Everything else:
              -- If it fits on one line, it fits
              -- If it fits on one line but with a newline after the `=`, it fits (including semicolon)
              -- Otherwise, start on new line, expand fully (including the semicolon)
              _ -> line <> group expr

-- Pretty a set
-- while we already pretty eagerly expand sets with more than one element,
-- in some situations even that is not sufficient. The wide parameter will
-- be even more eager at expanding, except for empty sets and inherit statements.
prettySet :: Bool -> (Maybe Leaf, Leaf, Items Binder, Leaf) -> Doc
-- Empty, non-recursive attribute set
prettySet _ (Nothing, Ann [] paropen Nothing, Items [], parclose@(Ann [] _ _))
    = pretty paropen <> pretty parclose
-- Singleton sets are allowed to fit onto one line,
-- but apart from that always expand.
prettySet wide (krec, Ann pre paropen post, binders, parclose)
    = base $ pretty (fmap (, hardspace) krec) <>
        pretty (Ann pre paropen Nothing)
        <> (surroundWith sep $ nest 2 $ pretty post <> prettyItems hardline binders)
        <> pretty parclose
    where
        sep = if wide && not (null (unItems binders)) then hardline else line'

prettyTermWide :: Term -> Doc
prettyTermWide (Set krec paropen items parclose) = prettySet True (krec, paropen, items, parclose)
prettyTermWide t = prettyTerm t

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
prettyTerm (List (Ann leading paropen Nothing) (Items []) (Ann [] parclose trailing'))
    = pretty leading <> pretty paropen <> pretty parclose <> pretty trailing'

-- General list
-- Always expand if len > 1
prettyTerm (List (Ann pre paropen post) items parclose) =
    base $ pretty (Ann pre paropen Nothing)
    <> (surroundWith line' $ nest 2 $ pretty post <> prettyItems hardline items)
    <> pretty parclose

prettyTerm (Set krec paropen items parclose) = prettySet False (krec, paropen, items, parclose)

-- Parenthesized application
prettyTerm (Parenthesized (Ann pre paropen post) (Application f a) parclose)
    = base $ group $ pretty (Ann pre paropen Nothing) <> nest 2 (
            -- Move comment trailing on '(' to next line, combine with comment from application
            case pretty post of { [] -> []; c -> hardline <> c }
            <> base (prettyApp hardline mempty line' hardline f a)
            <> case pretty post of  { [] -> mempty; _ -> hardline }
        ) <> pretty parclose

-- Parentheses
prettyTerm (Parenthesized paropen expr parclose)
    = base $ group $ pretty paropen <> lineL <> nest 2 (group expr) <> lineR <> pretty parclose
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

instance Pretty Term where
    pretty l@List{} = group $ prettyTerm l
    pretty x        = prettyTerm x

-- Does not move around comments, nor does it inject a trailing comma
instance Pretty ParamAttr where
    -- Simple parameter (no default)
    pretty (ParamAttr name Nothing maybeComma)
        = pretty name <> pretty maybeComma

    -- With ? default
    pretty (ParamAttr name (Just (qmark, def)) maybeComma)
        = group (pretty name <> hardspace <> pretty qmark
            <> absorb softline mempty (Just 2) def)
            <> pretty maybeComma

    -- `...`
    pretty (ParamEllipsis ellipsis)
        = pretty ellipsis

-- Move comments around when switching from leading comma to trailing comma style:
-- `, name # foo` → `name, #foo`
-- This only works for lines where the comma does not already have comments associated with it
-- This assumes that all items already have a trailing comma from earlier pre-processing
moveParamAttrComment :: ParamAttr -> ParamAttr
-- Simple parameter
moveParamAttrComment (ParamAttr (Ann trivia name (Just comment')) Nothing (Just (Ann [] comma Nothing)))
    = ParamAttr (Ann trivia name Nothing) Nothing (Just (Ann [] comma (Just comment')))
-- Parameter with default value
moveParamAttrComment (ParamAttr name (Just (qmark, def)) (Just (Ann [] comma Nothing)))
    = ParamAttr name (Just (qmark, def')) (Just (Ann [] comma comment'))
    where
        -- Extract comment at the end of the line
        (def', comment') = mapLastToken' (\case
            (Ann trivia t (Just comment'')) -> (Ann trivia t Nothing, Just comment'')
            ann -> (ann, Nothing)
            ) def
moveParamAttrComment x = x

-- When a `, name` entry has some line comments before it, they are actually attached to the comment
-- of the preceding item. Move them to the next one
-- Also adds the trailing comma on the last element if necessary
moveParamsComments :: [ParamAttr] -> [ParamAttr]
moveParamsComments
    -- , name1
    -- # comment
    -- , name2
    ((ParamAttr name maybeDefault (Just (Ann trivia comma Nothing))) :
     (ParamAttr (Ann trivia' name' Nothing) maybeDefault' maybeComma') :
    xs)
    = (ParamAttr name maybeDefault (Just (Ann [] comma Nothing)))
    : moveParamsComments ((ParamAttr (Ann (trivia ++ trivia') name' Nothing) maybeDefault' maybeComma') : xs)
-- This may seem like a nonsensical case, but keep in mind that blank lines also count as comments (trivia)
moveParamsComments
    -- , name
    -- # comment
    -- ellipsis
    [(ParamAttr name maybeDefault (Just (Ann trivia comma Nothing)))
    ,(ParamEllipsis (Ann trivia' name' trailing'))]
    = [(ParamAttr name maybeDefault (Just (Ann [] comma Nothing)))
    , (ParamEllipsis (Ann (trivia ++ trivia') name' trailing'))]
-- Inject a trailing comma on the last element if nessecary
moveParamsComments [(ParamAttr name def Nothing)] = [ParamAttr name def (Just (Ann [] TComma Nothing))]
moveParamsComments (x : xs) = x : moveParamsComments xs
moveParamsComments [] = []

instance Pretty Parameter where
    -- param:
    pretty (IDParameter i) = pretty i

    -- {}:
    pretty (SetParameter bopen [] bclose)
        = group $ pretty bopen <> pretty bclose

    -- { stuff }:
    pretty (SetParameter bopen attrs bclose) =
        group $
            pretty bopen
            <> (surroundWith sep $ nest 2 $ sepBy (sep<>hardspace) $ handleTrailingComma $ map moveParamAttrComment $ moveParamsComments $ attrs)
            <> pretty bclose
        where
        -- pretty all ParamAttrs, but mark the trailing comma of the last element specially
        -- This is so that the trailing comma will only be printed in the expanded form
        handleTrailingComma :: [ParamAttr] -> [Doc]
        handleTrailingComma [] = []
        -- That's the case we're interested in
        handleTrailingComma [(ParamAttr name maybeDefault (Just (Ann [] TComma Nothing)))]
            = [pretty (ParamAttr name maybeDefault Nothing) <> trailing ","]
        handleTrailingComma (x:xs) = pretty x : handleTrailingComma xs

        sep = case attrs of
            [] -> line
            [ParamEllipsis _] -> line'
            -- Attributes must be without default
            [ParamAttr _ Nothing _] -> line'
            [ParamAttr _ Nothing _, ParamEllipsis _] -> line'
            [ParamAttr _ Nothing _, ParamAttr _ Nothing _] -> line'
            [ParamAttr _ Nothing _, ParamAttr _ Nothing _, ParamEllipsis _] -> line'
            _ -> hardline

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
            = group' True $ nest 2 $ base $ pretty (Ann pre' open Nothing)
                <> (surroundWith line' $ group $ nest 2 $ pretty post' <> pretty expr)
                <> pretty close
        absorbLast arg = group' False $ nest 2 $ pretty arg

        -- Extract comment before the first function and move it out, to prevent functions being force-expanded
        (fWithoutComment, comment') = mapFirstToken' (\(Ann leading token trailing') -> (Ann [] token trailing', leading)) f

        renderedF = pre <> group (absorbApp fWithoutComment)
        renderedFUnexpanded = unexpandSpacing' Nothing renderedF
      in
        (if null comment' then mempty else commentPre)
        <> pretty comment' <> (
            if isSimple (Application f a) && isJust (renderedFUnexpanded) then
                (group' False $ fromJust renderedFUnexpanded <> hardspace <> absorbLast a <> post)
            else
                (group' False $ renderedF <> line <> absorbLast a <> post)
        ) <> (if null comment' then mempty else commentPost)

isAbstractionWithAbsorbableTerm :: Expression -> Bool
isAbstractionWithAbsorbableTerm (Abstraction (IDParameter _) _ (Term t)) | isAbsorbable t = True
isAbstractionWithAbsorbableTerm (Abstraction (IDParameter _) _ body) = isAbstractionWithAbsorbableTerm body
isAbstractionWithAbsorbableTerm _ = False

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
    pretty (Let let_ binders (Ann leading in_ trailing') expr)
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

          letPart = group $ pretty let_ <> hardline <> letBody
          letBody = nest 2 $ prettyItems hardline (Items bindersWithoutComments)
          inPart = group $ pretty (Ann [] in_ Nothing) <> hardline
              -- Take our trailing and inject it between `in` and body
              <> pretty (concat binderComments ++ leading ++ convertTrailing trailing')
              <> pretty expr

    pretty (Assert assert cond semicolon expr)
        = base (pretty assert <> hardspace
          <> nest 2 (group cond) <> pretty semicolon)
          <> absorbSet expr

    pretty (If if_ cond then_ expr0 else_ expr1)
        = base $ group' False $
            -- `if cond then` if it fits on one line, otherwise `if\n  cond\nthen` (with cond indented)
            group (pretty if_ <> line <> nest 2 (pretty cond) <> line <> pretty then_)
            <> (surroundWith line $ nest 2 $ group expr0)
            <> pretty else_ <> absorbElse expr1

    pretty (Abstraction (IDParameter param) colon body)
        = pretty param <> pretty colon <> absorbAbs 1 body
        where absorbAbs :: Int -> Expression -> Doc
              absorbAbs depth (Abstraction (IDParameter param0) colon0 body0) =
                  hardspace <> pretty param0 <> pretty colon0 <> absorbAbs (depth + 1) body0
              absorbAbs depth x
                  | depth <= 2 = absorbSet x
                  | otherwise = absorb hardline mempty Nothing x

    pretty (Abstraction param colon body)
        = pretty param <> pretty colon <> line <> pretty body

    pretty (Application f a)
        = prettyApp mempty mempty mempty mempty f a

    -- not chainable binary operators: <, >, <=, >=, ==, !=
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
            absorbOperation (Application f a) = group $ prettyApp hardline line mempty mempty f a
            absorbOperation x = hardspace <> pretty x

            prettyOperation :: (Maybe Leaf, Expression) -> Doc
            -- First element
            prettyOperation (Nothing, expr) = pretty expr
            -- The others
            prettyOperation ((Just op'), expr) =
                line <> pretty (moveTrailingCommentUp op') <> nest 2 (absorbOperation expr)
          in
            group' False $
                (concat . map prettyOperation . (flatten Nothing)) operation

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
isSimple (Term (String (Ann [] _ Nothing))) = True
isSimple (Term (Path (Ann [] _ Nothing))) = True
isSimple (Term (Token (Ann [] (Identifier _) Nothing))) = True
isSimple (Term (Selection t selectors))
    = isSimple (Term t) && all isSimpleSelector selectors
-- Function applications of simple terms are simple up to two arguments
isSimple (Application (Application (Application _ _) _) _) = False
isSimple (Application f a) = isSimple f && isSimple a
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

    -- Absorb terms
    -- This is exceedingly rare (why would one do this anyways?); one instance in the entire Nixpkgs
    pretty (Interpolation (Whole (Term t) []))
        | isAbsorbable t
            = group $ text "${" <> prettyTerm t <> text "}"

    -- For "simple" interpolations (see isSimple, but mostly just identifiers),
    -- force onto one line, regardless of length
    pretty (Interpolation (Whole expr []))
        | isSimple expr
            = text "${" <> fromMaybe (pretty expr) (unexpandSpacing' Nothing (pretty expr)) <> text "}"

    -- For interpolations, we try to render the content, to see how long it will be.
    -- If the interpolation is single-line and shorter than 30 characters, we force it
    -- onto that line, even if this would make it go over the line limit.
    pretty (Interpolation whole) =
        group $ text "${" <> inner <> text "}"
        where
        whole' = pretty whole
        inner = fromMaybe
            -- default
            (surroundWith line' $ nest 2 $ whole')
            -- force on one line if possible
            (unexpandSpacing' (Just 30) whole')

instance Pretty [StringPart] where
    -- When the interpolation is the only thing on the string line,
    -- then absorb the content (i.e. don't surround with line')
    pretty [Interpolation expr]
        = group $ text "${" <> pretty expr <> text "}"

    -- If we split a string line over multiple code lines due to large
    -- interpolations, make sure to indent based on the indentation of the line
    -- in the string.
    pretty (TextPart t : parts)
        = text t <> base (nest indentation (hcat parts))
        where indentation = textWidth $ Text.takeWhile isSpace t

    pretty parts = base $ hcat parts

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
    text "''"
    -- Usually the `''` is followed by a potential line break.
    -- However, for single-line strings it should be omitted, because often times a line break will
    -- not reduce the indentation at all
    <> (case parts of { _:_:_ -> line'; _ -> mempty })
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
