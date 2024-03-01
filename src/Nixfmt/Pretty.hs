{- © 2019 Serokell <hi@serokell.io>
 - © 2019 Lars Jellema <lars.jellema@gmail.com>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE FlexibleInstances, OverloadedStrings, RankNTypes, TupleSections, LambdaCase #-}

module Nixfmt.Pretty where

import Prelude hiding (String)

import Data.Char (isSpace)
import Data.Maybe (fromMaybe, isJust, fromJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as Text (null, takeWhile)

-- import Debug.Trace (traceShowId)
import Nixfmt.Predoc
  (Doc, GroupAnn(..), Pretty, emptyline, group, group', hardline, hardspace, hcat, line, line',
  nest, offset, newline, pretty, sepBy, surroundWith, softline, softline', text, comment, trailingComment, trailing, textWidth,
  unexpandSpacing')
import Nixfmt.Types
  (Ann(..), Binder(..), Expression(..), Item(..), Items(..), Leaf,
  ParamAttr(..), Parameter(..), Selector(..), SimpleSelector(..),
  StringPart(..), Term(..), Token(..), TrailingComment(..), Trivium(..),
  Whole(..), tokenText, mapFirstToken, mapFirstToken', mapLastToken', hasTrivia)

toLineComment :: TrailingComment -> Trivium
toLineComment (TrailingComment c) = LineComment $ " " <> c

-- If the token has some trailing comment after it, move that in front of the token
moveTrailingCommentUp :: Ann a -> Ann a
moveTrailingCommentUp (Ann pre a (Just post)) = Ann (pre ++ [toLineComment post]) a Nothing
moveTrailingCommentUp a = a

instance Pretty TrailingComment where
    pretty (TrailingComment c)
        = hardspace <> trailingComment ("# " <> c) <> hardline

instance Pretty Trivium where
    pretty EmptyLine        = emptyline
    pretty (LineComment c)  = comment ("#" <> c) <> hardline
    pretty (BlockComment isDoc c) =
              comment (if isDoc then "/**" else "/*") <> hardline
              -- Indent the comment using offset instead of nest
              <> (offset 2 $ hcat $ map prettyCommentLine c)
              <> comment "*/" <> hardline
        where
        prettyCommentLine :: Text -> Doc
        prettyCommentLine l
            | Text.null l = emptyline
            | otherwise   = comment l <> hardline


instance Pretty a => Pretty (Item a) where
    pretty (DetachedComments trivia) = pretty trivia
    pretty (CommentedItem trivia x) = pretty trivia <> group x

-- For lists, attribute sets and let bindings
prettyItems :: Pretty a => Doc -> Items a -> Doc
-- Special case: Preserve an empty line with no items
-- usually, trailing newlines after the last element are not preserved
prettyItems _ (Items [DetachedComments []]) = emptyline
prettyItems sep items = prettyItems' $ unItems items
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
        = pretty dot <> pretty sel
          <> softline <> nest (pretty kw <> hardspace <> pretty def)

-- in attrsets and let bindings
instance Pretty Binder where
    -- `inherit bar` statement
    pretty (Inherit inherit Nothing ids semicolon)
        = group $ pretty inherit
            <> (if null ids then pretty semicolon else
                line <> nest (sepBy (if length ids < 4 then line else hardline) ids <> line' <> pretty semicolon)
            )

    -- `inherit (foo) bar` statement
    pretty (Inherit inherit (Just source) ids semicolon)
        = group $ pretty inherit <> nest (
            (group' RegularG (line <> pretty source))
            <> if null ids then pretty semicolon else line
            <> sepBy (if length ids < 4 then line else hardline) ids
            <> line' <> pretty semicolon
        )

    -- `foo = bar`
    pretty (Assignment selectors assign expr semicolon)
        = group $ hcat selectors
                 <> nest (hardspace <> pretty assign <> nest (absorbRHS expr)) <> pretty semicolon

-- Pretty a set
-- while we already pretty eagerly expand sets with more than one element,
-- in some situations even that is not sufficient. The wide parameter will
-- be even more eager at expanding, except for empty sets and inherit statements.
prettySet :: Bool -> (Maybe Leaf, Leaf, Items Binder, Leaf) -> Doc
-- Empty attribute set
prettySet _ (krec, Ann [] paropen Nothing, Items [], parclose@(Ann [] _ _))
    = pretty (fmap (, hardspace) krec) <> pretty paropen <> hardspace <> pretty parclose
-- Singleton sets are allowed to fit onto one line,
-- but apart from that always expand.
prettySet wide (krec, Ann pre paropen post, binders, parclose)
    = pretty (fmap (, hardspace) krec) <>
        pretty (Ann pre paropen Nothing)
        <> (surroundWith sep $ nest $ pretty post <> prettyItems hardline binders)
        <> pretty parclose
    where
        sep = if wide && not (null (unItems binders)) then hardline else line

prettyTermWide :: Term -> Doc
prettyTermWide (Set krec paropen items parclose) = prettySet True (krec, paropen, items, parclose)
prettyTermWide t = prettyTerm t

-- | Pretty print a term without wrapping it in a group.
prettyTerm :: Term -> Doc
prettyTerm (Token t) = pretty t
prettyTerm (SimpleString (Ann leading s trailing')) = pretty leading <> prettySimpleString s <> pretty trailing'
prettyTerm (IndentedString (Ann leading s trailing')) = pretty leading <> prettyIndentedString s <> pretty trailing'
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
    = pretty leading <> pretty paropen <> hardspace <> pretty parclose <> pretty trailing'

-- General list
-- Always expand if len > 1
prettyTerm (List (Ann pre paropen post) items parclose) =
    pretty (Ann pre paropen Nothing)
    <> (surroundWith line $ nest $ pretty post <> prettyItems hardline items)
    <> pretty parclose

prettyTerm (Set krec paropen items parclose) = prettySet False (krec, paropen, items, parclose)

-- Parentheses
prettyTerm (Parenthesized paropen expr (Ann closePre parclose closePost))
    = group $
        pretty (moveTrailingCommentUp paropen)
        <> nest (inner <> pretty closePre)
        <> pretty (Ann [] parclose closePost)
  where
    inner =
      case expr of
        -- Start on the same line for these
        _ | isAbsorbableExpr expr -> group $ absorbExpr False expr
        -- Parenthesized application
        (Application f a) -> prettyApp True mempty True f a
        -- Same thing for selections
        (Term (Selection t _)) | isAbsorbable t -> line' <> group expr <> line'
        (Term (Selection _ _)) -> group expr <> line'
        -- Start on a new line for the others
        _ -> line' <> group expr <> line'

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
        = group $
            pretty name <> hardspace
            <> nest (pretty qmark <> nest (absorbRHS def))
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
        = group $ pretty (moveTrailingCommentUp bopen) <> hardspace <> pretty bclose

    -- { stuff }:
    pretty (SetParameter bopen attrs bclose) =
        group $
            pretty (moveTrailingCommentUp bopen)
            <> (surroundWith sep $ nest $ sepBy sep $ handleTrailingComma $ map moveParamAttrComment $ moveParamsComments $ attrs)
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
            [ParamEllipsis _] -> line
            -- Attributes must be without default
            [ParamAttr _ Nothing _] -> line
            [ParamAttr _ Nothing _, ParamEllipsis _] -> line
            [ParamAttr _ Nothing _, ParamAttr _ Nothing _] -> line
            [ParamAttr _ Nothing _, ParamAttr _ Nothing _, ParamEllipsis _] -> line
            _ -> hardline

    pretty (ContextParameter param1 at param2)
        = pretty param1 <> pretty at <> pretty param2


-- Function application
-- Some example mapping of Nix code to Doc (using brackets as groups, but omitting the outermost group
-- and groups around the expressions for conciseness):
--
-- `f a` -> pre f line a post
-- `f g a` -> pre [f line g] line a post
-- `f g h a` -> pre [[f line g] line h] line a post
-- `f g h i a` -> pre [[[f line g] line h] line i] line a post
--
-- As you can see, it separates the elements by `line` whitespace. However, there are several tricks to make it look good:
-- 1. For each function call (imagine the fully parenthesised Nix code), we group it. Due to the greedy expansion
--    of groups this means that it will place as many function calls on the first line as possible, but then all the remaining
--    ones on a separate line each.
-- 2. Each argument is declared as "priority" group, meaning that the layouting algorithm will try to expand
--    it first when things do not fit onto one line. If there are multiple arguments, they will each be attempted to
--    expand, individually and in reverse order (last argument first).
--    This allows the last argument to be multi-line without forcing the
--    preceding arguments to be multiline. This also allows other arguments to be multi-line as long
--    all remaining arguments fit onto a single line together
-- 3. Callers may inject `pre` and `post` tokens (mostly newlines) into the inside of the group.
--    This means that callers can say "try to be compact first, but if more than the last argument does not fit onto the line,
--    then start on a new line instead".
prettyApp :: Bool -> Doc -> Bool -> Expression -> Expression -> Doc
prettyApp indentFunction pre hasPost f a
    = let
        absorbApp (Application f' a') = (group' Transparent $ absorbApp f') <> line <> (nest (group' Priority a'))
        absorbApp expr
            | indentFunction && (null comment') = nest $ group' RegularG $ line' <> pretty expr
            | otherwise = pretty expr

        absorbLast (Term t) | isAbsorbable t
            = group' Priority $ nest $ prettyTerm t
        -- Special case: Absorb parenthesized function declaration with absorbable body
        absorbLast
            (Term (Parenthesized
                open (Abstraction (IDParameter name) colon (Term body)) close
            ))
            | isAbsorbableTerm body && all (not . hasTrivia) [open, name, colon]
            = group' Priority $ nest $
                pretty open <> pretty name <> pretty colon <> hardspace
                <> prettyTermWide body
                <> pretty close
        -- Special case: Absorb parenthesized function application with absorbable body
        absorbLast
            (Term (Parenthesized
                open (Application (Term (Token ident@(Ann _ fn@(Identifier _) _))) (Term body)) close
            ))
            | isAbsorbableTerm body && all (not . hasTrivia) [open, ident, close]
            = group' Priority $ nest $
                pretty open <> pretty fn <> hardspace
                <> prettyTermWide body
                <> pretty close
        absorbLast (Term (Parenthesized open expr close))
            = absorbParen open expr close
        absorbLast arg = group' RegularG $ nest $ pretty arg

        -- Extract comment before the first function and move it out, to prevent functions being force-expanded
        (fWithoutComment, comment') = mapFirstToken'
            ((\(Ann leading token trailing') -> (Ann [] token trailing', leading)) . moveTrailingCommentUp)
            f

        renderedF = pre <> group' Transparent (absorbApp fWithoutComment)
        renderedFUnexpanded = unexpandSpacing' Nothing renderedF

        post = if hasPost then line' else mempty
      in
        pretty comment'
        <> (
            if isSimple (Application f a) && isJust (renderedFUnexpanded) then
                (group' RegularG $ fromJust renderedFUnexpanded <> hardspace <> absorbLast a)
            else
                (group' RegularG $ renderedF <> line <> absorbLast a <> post)
        )
        <> (if hasPost && not (null comment') then hardline else mempty)

prettyWith :: Bool -> Expression -> Doc
-- absorb the body
prettyWith True (With with expr0 semicolon (Term expr1))
        = group' RegularG $
            line' <>
            pretty with <> hardspace
            <> nest (group expr0) <> pretty semicolon
            -- Force-expand attrsets
            <> hardspace <> group' Priority (prettyTermWide expr1)
-- Normal case
prettyWith _ (With with expr0 semicolon expr1)
        = group (
            pretty with <> hardspace
            <> nest (group expr0) <> pretty semicolon
          )
          <> line <> pretty expr1
prettyWith _ _ = error "unreachable"

isAbsorbableExpr :: Expression -> Bool
isAbsorbableExpr expr = case expr of
    (Term t) | isAbsorbableTerm t -> True
    (With _ _ _ (Term t)) | isAbsorbableTerm t -> True
    -- Absorb function declarations but only those with simple parameter(s)
    (Abstraction (IDParameter _) _ (Term t)) | isAbsorbable t -> True
    (Abstraction (IDParameter _) _ body@(Abstraction _ _ _)) -> isAbsorbableExpr body
    _ -> False

isAbsorbable :: Term -> Bool
-- Multi-line indented string
isAbsorbable (IndentedString (Ann _ (_:_:_) _)) = True
isAbsorbable (Path _) = True
-- Non-empty sets and lists
isAbsorbable (Set _ _ (Items (_:_)) _)                                   = True
isAbsorbable (List _ (Items (_:_)) _)                                    = True
isAbsorbable (Parenthesized (Ann [] _ Nothing) (Term t) _)               = isAbsorbable t
isAbsorbable _                                                           = False

isAbsorbableTerm :: Term -> Bool
isAbsorbableTerm = isAbsorbable

absorbParen :: Ann Token -> Expression -> Ann Token -> Doc
absorbParen (Ann pre' open post') expr (Ann pre'' close post'')
    = group' Priority $ nest $ pretty (Ann pre' open Nothing)
        -- Move any trailing comments on the opening parenthesis down into the body
        <> (surroundWith line' $ group' RegularG $ nest $
            pretty (mapFirstToken
                (\(Ann leading token trailing') -> (Ann (maybeToList (toLineComment <$> post') ++ leading) token trailing'))
                expr)
            -- Move any leading comments on the closing parenthesis up into the nest
            <> pretty pre''
        )
        <> pretty (Ann [] close post'')

-- Note that unlike for absorbable terms which can be force-absorbed, some expressions
-- may turn out to not be absorbable. In that case, they should start with a line' so that
-- they properly start on the next line if necessary.
absorbExpr :: Bool -> Expression -> Doc
absorbExpr True (Term t) | isAbsorbableTerm t = prettyTermWide t
absorbExpr False (Term t) | isAbsorbableTerm t = prettyTerm t
-- With expression with absorbable body: Treat as absorbable term
absorbExpr _ expr@(With _ _ _ (Term t)) | isAbsorbableTerm t = prettyWith True expr
absorbExpr _ expr = pretty expr

-- Render the RHS value of an assignment or function parameter default value
absorbRHS :: Expression -> Doc
absorbRHS expr = case expr of
    -- Absorbable expression. Always start on the same line
    _ | isAbsorbableExpr expr -> hardspace <> group (absorbExpr True expr)
    -- Parenthesized expression. Same thing as the special case for parenthesized last argument in function calls.
    (Term (Parenthesized open expr' close)) -> hardspace <> absorbParen open expr' close
    -- Not all strings are absorbable, but in this case we always want to keep them attached.
    -- Because there's nothing to gain from having them start on a new line.
    (Term (SimpleString _)) -> hardspace <> group expr
    (Term (IndentedString _)) -> hardspace <> group expr
    -- Same for path
    (Term (Path _)) -> hardspace <> group expr
    -- Non-absorbable term
    -- If it is multi-line, force it to start on a new line with indentation
    (Term _) -> group' RegularG (line <> pretty expr)
    -- Function call
    -- Absorb if all arguments except the last fit into the line, start on new line otherwise
    (Application f a) -> prettyApp False line False f a
    (With _ _ _ _) -> group' RegularG $ line <> pretty expr
    -- Special case `//` and `++` operations to be more compact in some cases
    -- Case 1: two arguments, LHS is absorbable term, RHS fits onto the last line
    (Operation (Term t) (Ann [] op Nothing) b) | isAbsorbable t && isUpdateOrConcat op ->
      group' RegularG $ line <> group' Priority (prettyTermWide t) <> line <> pretty op <> hardspace <> pretty b
    -- Case 2a: LHS fits onto first line, RHS is an absorbable term
    (Operation l (Ann [] op Nothing) (Term t)) | isAbsorbable t && isUpdateOrConcat op ->
      group' RegularG $ line <> pretty l <> line <> group' Transparent (pretty op <> hardspace <> group' Priority (prettyTermWide t))
    -- Case 2b: LHS fits onto first line, RHS is a function application
    (Operation l (Ann [] op Nothing) (Application f a)) | isUpdateOrConcat op ->
      line <> (group l) <> line <> prettyApp False (pretty op <> hardspace) False f a
    -- Everything else:
    -- If it fits on one line, it fits
    -- If it fits on one line but with a newline after the `=`, it fits (including semicolon)
    -- Otherwise, start on new line, expand fully (including the semicolon)
    _ -> line <> group expr

    where
        isUpdateOrConcat TUpdate = True
        isUpdateOrConcat TConcat = True
        isUpdateOrConcat _ = False

instance Pretty Expression where
    pretty (Term t) = pretty t

    pretty with@(With _ _ _ _) = prettyWith False with

    -- Let bindings are always fully expanded (no single-line form)
    -- We also take the comments around the `in` (trailing, leading and detached binder comments)
    -- and move them down to the first token of the body
    pretty (Let let_ binders (Ann leading in_ trailing') expr)
        = letPart <> hardline <> inPart
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
          letBody = nest $ prettyItems hardline (Items bindersWithoutComments)
          inPart = group $ pretty (Ann [] in_ Nothing) <> hardline
              -- Take our trailing and inject it between `in` and body
              <> pretty (concat binderComments ++ leading ++ convertTrailing trailing')
              <> pretty expr

    pretty (Assert assert cond semicolon expr)
        = group $
            -- Render the assert as if it is was just a function (literally)
            uncurry (prettyApp False mempty False) (insertIntoApp (Term $ Token assert) cond)
            <> pretty semicolon <> hardline <> pretty expr
            where
                -- Add something to the left of a function application
                -- We need to walk down the arguments here because applications are left-associative.
                insertIntoApp :: Expression -> Expression -> (Expression, Expression)
                insertIntoApp insert (Application f a) = ((uncurry Application $ insertIntoApp insert f), a)
                insertIntoApp insert other = (insert, other)

    pretty expr@(If _ _ _ _ _ _)
        -- If the first `if` or any `else` has a trailing comment, move it up.
        -- However, don't any subsequent `if` (`else if`). We could do that, but that
        -- would require taking care of edge cases which are not worth handling.
        = group' RegularG $ prettyIf line $ mapFirstToken moveTrailingCommentUp expr
        where
            -- Recurse to absorb nested "else if" chains
            prettyIf :: Doc -> Expression -> Doc
            prettyIf sep (If if_ cond then_ expr0 else_ expr1)
                -- `if cond then` if it fits on one line, otherwise `if\n  cond\nthen` (with cond indented)
                = group (pretty if_ <> line <> nest (pretty cond) <> line <> pretty then_)
                <> (surroundWith sep $ nest $ group expr0)
                -- Using hardline here is okay because it will only apply to nested ifs, which should not be inline anyways.
                <> pretty (moveTrailingCommentUp else_) <> hardspace <> prettyIf hardline expr1
            prettyIf _ x
                = line <> nest (group x)

    -- Simple parameter
    pretty (Abstraction (IDParameter param) colon body)
        = group' RegularG $ line' <> pretty param <> pretty colon <> absorbAbs 1 body
        where
            absorbAbs :: Int -> Expression -> Doc
            -- If there are multiple ID parameters to that function, treat them all at once
            absorbAbs depth (Abstraction (IDParameter param0) colon0 body0) =
                hardspace <> pretty param0 <> pretty colon0 <> absorbAbs (depth + 1) body0
            absorbAbs _ expr | isAbsorbableExpr expr = hardspace <> group' Priority (absorbExpr False expr)
            -- Force the content onto a new line when it is not absorbable and there are more than two arguments
            absorbAbs depth x
                = (if depth <= 2 then line else hardline) <> pretty x

    -- Attrset parameter
    pretty (Abstraction param colon (Term t))
        | isAbsorbable t
        = pretty param <> pretty colon <> line <> group (prettyTermWide t)
    pretty (Abstraction param colon body)
        = pretty param <> pretty colon <> line <> pretty body

    pretty (Application f a)
        = prettyApp False mempty False f a

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
            absorbOperation (Term t) | isAbsorbable t = hardspace <> (pretty t)
            -- Force nested operations to start on a new line
            absorbOperation x@(Operation _ _ _) = group' RegularG $ line <> pretty x
            -- Force applications to start on a new line if more than the last argument is multiline
            absorbOperation (Application f a) = group $ prettyApp False line False f a
            absorbOperation x = hardspace <> pretty x

            prettyOperation :: (Maybe Leaf, Expression) -> Doc
            -- First element
            prettyOperation (Nothing, expr) = pretty expr
            -- The others
            prettyOperation ((Just op'), expr) =
                line <> pretty (moveTrailingCommentUp op') <> nest (absorbOperation expr)
          in
            group' RegularG $
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


isSimpleSelector :: Selector -> Bool
isSimpleSelector (Selector _ (IDSelector _) Nothing) = True
isSimpleSelector _                                   = False

isSimple :: Expression -> Bool
isSimple (Term (SimpleString (Ann [] _ Nothing))) = True
isSimple (Term (IndentedString (Ann [] _ Nothing))) = True
isSimple (Term (Path (Ann [] _ Nothing))) = True
isSimple (Term (Token (Ann [] (Identifier _) Nothing))) = True
isSimple (Term (Selection t selectors))
    = isSimple (Term t) && all isSimpleSelector selectors
isSimple (Term (Parenthesized (Ann [] _ Nothing) e (Ann [] _ Nothing))) = isSimple e
-- Function applications of simple terms are simple up to two arguments
isSimple (Application (Application (Application _ _) _) _) = False
isSimple (Application f a) = isSimple f && isSimple a
isSimple _ = False

-- STRINGS

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
            (surroundWith line' $ nest $ whole')
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
        = text t <> offset indentation (hcat parts)
        where indentation = textWidth $ Text.takeWhile isSpace t

    pretty parts = hcat parts

prettySimpleString :: [[StringPart]] -> Doc
prettySimpleString parts = group $
    text "\""
    -- Use literal \n here instead of `newline`, as the latter
    -- would cause multiline-string-style indentation which we do not want
    <> sepBy (text "\n") (map pretty parts)
    <> text "\""

prettyIndentedString :: [[StringPart]] -> Doc
prettyIndentedString parts = group $
    text "''"
    -- Usually the `''` is followed by a potential line break.
    -- However, for single-line strings it should be omitted, because often times a line break will
    -- not reduce the indentation at all
    <> (case parts of { _:_:_ -> line'; _ -> mempty })
    <> (nest $ sepBy newline $ map pretty parts)
    <> text "''"
