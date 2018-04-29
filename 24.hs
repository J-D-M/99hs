module RandSelect (randSelect) where
import System.Random

-- from part 23
ranSel :: [Int] -> Int -> IO[Int]
ranSel ls n = do
  gen <- getStdGen
  return $ take n $ randomRs(0, (length ls) - 1) gen

randSelect :: Int -> Int -> IO[Int]
randSelect amount max = ranSel [1..max] amount
