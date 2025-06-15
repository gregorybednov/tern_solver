module Ternary.Statement (Statement(..), st) where
import Ternary.Term (Vee, Term(..), Item(..))

data StatementKind = A | I | E | O | A' | I' | E' | O' deriving (Eq, Show, Read)
data Statement a = Statement StatementKind a a deriving (Eq, Read)

instance Show a => Show (Statement a) where
  show (Statement A x y) = "Every " ++ show x ++ " is " ++ show y
  show (Statement I x y) = "Some of " ++ show x ++ " is " ++ show y
  show (Statement E x y) = "None of " ++ show x ++ " is " ++ show y
  show (Statement O x y) = "Some of " ++ show x ++ " isn't " ++ show y
  show (Statement A' x y) = "Every non-" ++ show x ++ " is " ++ show y
  show (Statement I' x y) = "Some of non-" ++ show x ++ " is " ++ show y
  show (Statement E' x y) = "None of non-" ++ show x ++ " is " ++ show y
  show (Statement O' x y) = "Some of non-" ++ show x ++ " isn't " ++ show y

inv :: (Eq a) => Term a -> Term a
inv (Term p x) = Term (not p) x

i :: (Eq a) => Bool -> Term a -> Term a -> Vee a
i v x y
    | x == inv y  = error "x and not x under the same Vee, refusing to calculate"
    | x /= y      = Term v . Item $ [x, y]
    | x == y      = Term v . Item $ [x]

st :: (Eq a) => Statement a  -> Vee a
st (Statement A x y) = i False (Term True x) (Term False y)
st (Statement I x y) = i True (Term True x) (Term True y)
st (Statement E x y) = i False (Term True x) (Term True y)
st (Statement O x y) = i True (Term True x) (Term False y)
st (Statement A' x y) = i False (Term False x) (Term False y)
st (Statement I' x y) = i True (Term False x) (Term True y)
st (Statement E' x y) = i False (Term False x) (Term True y)
st (Statement O' x y) = i True (Term False x) (Term False y)
