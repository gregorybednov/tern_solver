module Ternary.Term (Term(..), Item(..), Vee) where
import           Data.List     (concatMap, null, (++), (\\))
import           Prelude       (Applicative, Bool (False, True), Eq, Functor,
                                Read, Show, String, fmap, map, pure, show,
                                (&&), (/=), (<*>), (==), (||))

data Term a = Term Bool a deriving (Read)
newtype Item a = Item [a] deriving (Read)
type Vee a = Term (Item (Term a))

instance Functor Item where
    fmap f (Item a) = Item (map f a)

instance Applicative Item where
    pure x = Item [x]
    (Item fs) <*> (Item xs) = Item [f x | f <- fs, x <- xs]

instance (Eq a) => Eq (Item a) where
    (Item x) == (Item y) = null dxy && null dyx
             where
               dxy = x \\ y
               dyx = y \\ x

instance (Eq a) => Eq (Term a) where
  Term v x == Term w y = (v==w) && (x==y)
  Term v x /= Term w y = (v/=w) || (x/=y)

instance Show (Term String) where
  show (Term False y) = y ++ "`"
  show (Term True y)  = y

instance Show (Term (Item (Term String))) where
    show (Term x i) = show (Term x "V") ++ concatMap show (it i)
                      where it (Item ii) = ii
