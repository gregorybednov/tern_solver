module Ternary.Statement (Statement(..), st) where
import Ternary.Term (Vee, Term(..), Item(..))
data Statement a = A a a -- Affirmo (general affirmative)
               | I a a -- affIrmo (private affirmative)
               | E a a -- nEgo    (general negative)
               | O a a -- negO    (private negative)
               | A' a a
               | I' a a
               | E' a a
               | O' a a
               deriving (Eq, Show, Read)

inv :: (Eq a) => Term a -> Term a
inv (Term p x) = Term (not p) x

i :: (Eq a) => Bool -> Term a -> Term a -> Vee a
i v x y
    | x == inv y  = error "x and not x under the same Vee, refusing to calculate"
    | x /= y      = Term v . Item $ [x, y]
    | x == y      = Term v . Item $ [x]

st :: (Eq a) => Statement a  -> Vee a
st (A x y) = i False (Term True x) (Term False y)
st (I x y) = i True (Term True x) (Term True y)
st (E x y) = i False (Term True x) (Term True y)
st (O x y) = i True (Term True x) (Term False y)
st (A' x y) = i False (Term False x) (Term False y)
st (I' x y) = i True (Term False x) (Term True y)
st (E' x y) = i False (Term False x) (Term True y)
st (O' x y) = i True (Term False x) (Term False y)
