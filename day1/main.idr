import System.File
import Data.List
import Data.List1
import Data.String

process : String -> String
process content =
    let values:List Integer = forget $ map(sum . maybe [0] id . sequence) $
        splitOn Nothing $ map parsePositive $ lines content
    in

    show $ take 3
         $ sortBy (\(_, x), (_, y) => compare y x)
         $ zip[1..length values] $ values


main : IO ()
main = do
  Right content <- readFile "input.txt" | Left err => printLn err
  putStr (process content)
