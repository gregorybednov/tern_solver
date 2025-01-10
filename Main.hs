module Main (main) where
import Prelude (IO, String, getContents, lines, map, print,
            otherwise, Bool(..), filter, id, unlines,
                mapM_, ($), elem, notElem, (||), putStrLn, read, (.))
import Ternary.Statement  (Statement (..), st)
import Ternary.Universum  (Universum (..), universum)
import Ternary.Vee        (cleared, think)
import System.Environment (getArgs)
import Data.Foldable (Foldable)

main2 :: Universum -> Bool -> IO ()
main2 u onlyNew = do
  strs <- getContents
  let statements = map str2vee . lines $ strs
      str2vee x = st (read x :: Statement String)
  mapM_ print
    . (if onlyNew then filter (`notElem` statements) else id)
    . cleared
    . think (universum u) $ statements

withArgs :: (String -> Bool) -> IO () 
withArgs a
  | a "-h" || a "--help" = putStrLn . unlines $
    ["Logical statement solver",
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
  | otherwise = main2 uni onlyNew where
     onlyNew = a "--new" || a "-n"
     uni =
       if a "-A" || a "--aristotle"
         then Aristotle
         else Empty

main :: IO ()
main = do
  args <- getArgs
  withArgs (`elem` args)
