module Main (main) where -- move to main dir and rename into Main
import Ternary.Statement (Statement (..), st)
import Ternary.Universum (Universum (..), universum)
import Ternary.Vee (isObvious, think, cleared)


sylloTest :: IO ()
sylloTest = do
  testStrs <- getContents
  let tests = map (\x -> (read x :: (Statement String, Statement String, Statement String)) ) . lines $ testStrs
  let run (_1, _2, _3)
       = all (\vb -> any (\va -> (vb == va) || isObvious vb va) a) b where
       a = think (universum Aristotle) $ map st [_1, _2]
       b = think (universum Aristotle) [st _3]
  putStrLn $ if all run tests
          then "✅ Passed"
          else "❌ Declined"

main = sylloTest
