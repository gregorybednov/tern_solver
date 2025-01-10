module Ternary.Vee (isObvious, newFact, cleared, think) where
import           Data.List    (head, intersect, length, nub, null, union, (\\))
import           Data.Maybe   (Maybe (Nothing), mapMaybe)
import           Prelude      (Bool (False, True), Eq, any, foldr, fst, map,
                               not, notElem, otherwise, return, snd, ($), (&&),
                               (.), (/=), (<$>), (<*>), (=<<), (==))
import           Ternary.Term (Item (..), Term (..), Vee)
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

newFact :: (Eq a) => Vee a -> Vee a -> ([Vee a], Maybe (Vee a))
newFact a@(Term True _) b@(Term False _) = newFact b a
newFact (Term False (Item iF)) tT@(Term True (Item iT))
    | length ldF /= 1 = ([], Nothing)
    | otherwise =
        if d'F `notElem` iT
        then (return tT, return (Term True (Item (d'F:iT))))
        else ([], Nothing)
      where
        ldF = iF \\ iT
        d'F = notT . head $ ldF
        notT (Term x v) = Term (not x) v
newFact (Term False (Item i0)) (Term False (Item i1))
    | length e /= 1 = ([], Nothing)
    | otherwise =
        if null d0 && null d1
        then (map (Term False . Item) [i0, i1], vee0)
        else ([], vee0)
    where
      notT (Term x v) = Term (not x) v
      terms = (i0 `intersect` i1) `union` d0 `union` d1
      e = map notT i0 `intersect` i1
      d0 = (i0 \\ i1) \\ map notT e
      d1 = (i1 \\ i0) \\ e
      vee0 = return (Term False (Item terms))
newFact _ _ = ([], Nothing)

pseudofix :: (Eq a) => (a -> a) -> a -> a
pseudofix f x0
 | y == y' = y
 | otherwise = y'
 where
  y  = f x0
  y' = f y

next :: (Eq a) => ([Vee a], [Vee a]) -> ([Vee a], [Vee a])
next (o,n) = (o `union` n \\ (fst =<< r), mapMaybe snd r)
 where
   r = newFact <$> o <*> n

applyFacts :: (Eq a) => [Vee a] -> [Vee a] -> [Vee a]
applyFacts old new = fst $ pseudofix next (old, new)

cleared :: (Eq a) => [Vee a] -> [Vee a]
cleared vees = nub [vee | vee <- vees, not $ any (isObvious vee) vees]

think :: (Eq a) => ([Vee a]->[Vee a]) -> [Vee a] -> [Vee a]
think addition = foldr (applyFacts . withUni . return) [] where
    withUni vees = applyFacts (addition vees) vees
