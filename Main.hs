module Main (main) where

import Data.Foldable (Foldable, any)
import Data.List (head, tail)
import Data.Maybe (isNothing)
import GHC.Base (Maybe (..), (==))
import System.Environment (getArgs)
import Ternary.Statement (Statement (..), st)
import Ternary.Term (Item (..), Term (..))
import Ternary.Universum (Universum (..), universum)
import Ternary.Vee (cleared, hasContradiction, think)
import Prelude
  ( Bool (..),
    IO,
    String,
    elem,
    filter,
    getContents,
    id,
    lines,
    map,
    mapM_,
    notElem,
    otherwise,
    print,
    putStrLn,
    read,
    unlines,
    ($),
    (.),
    (||),
  )

mainSolve :: Universum -> Bool -> IO ()
mainSolve u onlyNew = do
  strs <- getContents
  let statements = map str2vee . lines $ strs
      str2vee x = st (read x :: Statement String)
  mapM_ print
    . (if onlyNew then filter (`notElem` statements) else id)
    . cleared
    . think (universum u)
    $ statements

mainProve :: Universum -> Bool -> IO ()
mainProve u onlyNew = do
  strs <- getContents
  let statements = map str2vee . tail . lines $ strs
      proveThis = str2vee . head . lines $ strs
      str2vee x = st (read x :: Statement String)
      results = cleared . think (universum u) $ statements
      anyContradictions = any (hasContradiction proveThis) results
      proof = swapNothingJF $ if anyContradictions then Nothing else Just (Item recalc == Item results)
      recalc = cleared . think (universum u) $ proveThis : statements
      swapNothingJF x
        | isNothing x = Just False
        | x == Just False = Nothing
        | x == Just True = Just True

  print recalc
  putStrLn ""
  print results
  putStrLn ""
  print proof

withArgs :: (String -> Bool) -> IO ()
withArgs a
  | a "-h" || a "--help" =
      putStrLn . unlines $
        [ "Logical statement solver",
          "Usage: solver [PARAMETERS]",
          "",
          "Designed according to N.P.Brousentsov works",
          "and Lewis Carrol diagram. PARAMETERS:",
          "",
          "--aristotle, -A\tuse Aristotle logical universum VxVx' for each x",
          "--new, -n\tfilters only 'new' facts, excluding defined",
          "--help, -h\tshows this help",
          "--version, -v\tshows this version",
          "",
          "Takes statement \"Socrates is a human\" in such format, e.g.",
          "",
          "A \"Socrates\" \"human\"",
          "",
          "There are A, E, O, I traditional statements exist.",
          "Also there are A~, E~, O~, I~ with negated first part"
        ]
  | a "--version" || a "-v" = putStrLn "v1.0.2"
  | a "--prove" = mainProve Aristotle False
  | otherwise = mainSolve uni onlyNew
  where
    onlyNew = a "--new" || a "-n"
    uni =
      if a "-A" || a "--aristotle"
        then Aristotle
        else Empty

main :: IO ()
main = do
  args <- getArgs
  withArgs (`elem` args)
