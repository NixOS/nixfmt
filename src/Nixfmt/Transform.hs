{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Nixfmt.Transform (sortKeys) where
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Semigroup (Endo(..))
import Data.Text (Text)
import Nixfmt.Types

-- Normalize order of set keys are let assignments, transforming, for
-- example
--
-- let
--   d = 3;
--   c = 4;
--   e = 5;
-- in {
--   inherit e;
--   b = d;
--   a = c;
-- }
--
-- into
--
--   let
--     c = 4;
--     d = 3;
--     e = 5;
--   in {
--     a = c;
--     b = d;
--     inherit e;
--   }
--
-- Keys are sorted alphabetically, "inherit" expression moved below
-- assignments.

sortKeys :: Endo File
sortKeys = Endo $ \(File ann expr) -> File ann (sortKeysExpr expr)

-- XXX: Here I re-invent Data.Data.
omapT :: (Term -> Term) -> Term -> Term
omapT f = go
  where
    go = \case
      List x subs y -> f $ List x (map go subs) y
      e -> f e

omapE :: (Expression -> Expression) -> Expression -> Expression
omapE f = go
  where
    go = f . \case
      Term x -> Term x
      With l1 e1 l2 e2 -> With l1 (go e1) l2 (go e2)
      Let l1 b l2 e -> Let l1 b l2 (go e)
      Assert l1 e1 l2 e2 -> Assert l1 (go e1) l2 (go e2)
      If l1 e1 l2 e2 l3 e3 -> If l1 (go e1) l2 (go e2) l3 (go e3)
      Abstraction p l e -> Abstraction p l (go e)
      Application e1 e2 -> Application (go e1) (go e2)
      Operation e1 l e2 -> Operation (go e1) l (go e2)
      MemberCheck e1 l s -> MemberCheck (go e1) l s
      Negation l e -> Negation l (go e)
      Inversion l e -> Inversion l (go e)

sortKeysExpr :: Expression -> Expression
sortKeysExpr = omapE sortE
  where
    sortE = \case
      Let l1 b l2 e -> Let l1 (sortBinders b) l2 e;
      Term x -> Term $ omapT sortT x
      e -> e
    sortT = \case
      Set l1 l2 b l3 -> Set l1 l2 (sortBinders b) l3
      t -> t

sortBinders :: [Binder] -> [Binder]
sortBinders = sortBy (comparing keyB)
  where
    -- And here I reinvent lens.
    keyB :: Binder -> (Int, [(Int, Maybe Text)])
    keyB = \case
      Assignment sels _ _ _ -> (0, map keyS sels)
      Inherit _ _ names _   -> (1, map (\x -> (0, keyL x)) names)

    keyS :: Selector -> (Int, Maybe Text)
    keyS (Selector _ ss _) = keySS ss

    keySS :: SimpleSelector -> (Int, Maybe Text)
    keySS = \case
      -- Interpolation can contain arbitrary sub-expression and is too
      -- complicated to sort. Just put it below regular identifiers.
      IDSelector l -> (0, keyL l)
      _            -> (1, Nothing)

    keyL :: Leaf -> Maybe Text
    keyL (Ann t _ _) = keyT t

    keyT :: Token -> Maybe Text
    keyT = \case
      Identifier txt -> Just txt
      _ -> Nothing

