module Ternary.Universum (Universum(..), universum, aFromStatements) where
import           Data.List     (nub)
import           Ternary.Term  (Item (..), Term (..), Vee)

data Universum = Aristotle
               | Empty
               | Default

universum :: (Eq a) => Universum -> [Vee a] -> [Vee a]
universum Aristotle facts =
    [Term True (Item [Term x v])
         | v <- aFromStatements facts,
           x <- [False, True]]
universum Empty _ = []
universum Default xs = xs

aFromStatements :: (Eq a) => [Vee a] -> [a]
aFromStatements = nub . concatMap (extract . getItem . getVee)
  where
    getItem  (Item x) = x
    getVee (Term _ i) = i
    extract terms = [v | (Term _ v) <- terms]
