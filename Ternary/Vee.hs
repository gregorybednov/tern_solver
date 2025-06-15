module Ternary.Vee (isObvious, hasContradiction, newFact, cleared, think) where

import Data.Foldable (concatMap)
import Data.List
  ( elem,
    filter,
    head,
    intersect,
    iterate,
    length,
    nub,
    null,
    union,
    (\\),
  )
import Data.Maybe (Maybe (Nothing), mapMaybe)
import GHC.Base ((<), (>))
import GHC.Maybe (Maybe (..))
import Ternary.Term (Item (..), Term (..), Vee)
import Prelude
  ( Bool (False, True),
    Eq,
    any,
    foldr,
    fst,
    map,
    not,
    notElem,
    otherwise,
    return,
    snd,
    ($),
    (&&),
    (.),
    (/=),
    (<$>),
    (<*>),
    (=<<),
    (==),
  )

type Knowledge a = [Vee a]

type InferenceRule a = Vee a -> Vee a -> (Knowledge a, Maybe (Vee a))

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
a `isSubsetOf` b = nda && not ndb
  where
    nda = null $ a \\ b
    ndb = null $ b \\ a

isObvious :: (Eq a) => Vee a -> Vee a -> Bool
isObvious (Term x (Item a)) (Term y (Item b))
  | x /= y = False
  | x = a `isSubsetOf` b
  | not x = b `isSubsetOf` a

hasContradiction :: (Eq a) => Vee a -> Vee a -> Bool
hasContradiction (Term x (Item a)) (Term y (Item b))
  | a == b && x /= y = True
  | a `isSubsetOf` b && x < y = True
  | b `isSubsetOf` a && x > y = True
  | otherwise = False

notT :: Term a -> Term a
notT (Term x v) = Term (not x) v

rulePosFromNeg :: (Eq a) => InferenceRule a
rulePosFromNeg (Term False (Item negSet)) positive@(Term True (Item posSet))
  | length diff /= 1 = ([], Nothing)
  | missing `elem` posSet = ([], Nothing)
  | otherwise = ([positive], Just (Term True (Item (missing : posSet))))
  where
    diff = negSet \\ posSet
    missing = notT (head diff)

ruleNegFromNeg :: (Eq a) => InferenceRule a
ruleNegFromNeg (Term False (Item i0)) (Term False (Item i1))
  | length evidence /= 1 = ([], Nothing)
  | null diff0 && null diff1 = (map (Term False . Item) [i0, i1], Just (Term False (Item result)))
  | otherwise = ([], Just (Term False (Item result)))
  where
    evidence = map notT i0 `intersect` i1
    diff0 = (i0 \\ i1) \\ map notT evidence
    diff1 = (i1 \\ i0) \\ evidence
    result = (i0 `intersect` i1) `union` diff0 `union` diff1

newFact :: (Eq a) => InferenceRule a
newFact a@(Term True _) b@(Term False _) = newFact b a
newFact a@(Term False _) b@(Term True _) = rulePosFromNeg a b
newFact a@(Term False _) b@(Term False _) = ruleNegFromNeg a b
newFact _ _ = ([], Nothing)

next :: (Eq a) => (Knowledge a, Knowledge a) -> (Knowledge a, Knowledge a)
next (old, new) = (old `union` new \\ used, added)
  where
    results = [newFact o n | o <- old, n <- new]
    used = concatMap fst results
    added = mapMaybe snd results

applyFacts :: (Eq a) => Knowledge a -> Knowledge a -> Knowledge a
applyFacts old new =
  fst . head . dropStable $ iterate next (old, new)
  where
    dropStable (x : y : xs)
      | x == y = [x]
      | otherwise = dropStable (y : xs)
    dropStable _ = []

cleared :: (Eq a) => Knowledge a -> Knowledge a
cleared vees = nub $ filter (not . isRedundant) vees
  where
    isRedundant v = any (isObvious v) vees

think :: (Eq a) => (Knowledge a -> Knowledge a) -> Knowledge a -> Knowledge a
think addition = foldr (applyFacts . withUni . return) []
  where
    withUni vees = applyFacts (addition vees) vees
