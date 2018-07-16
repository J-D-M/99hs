module RanSel
  ( ranSel
  ) where

import System.Random

ranSel ls n = do
  gen <- getStdGen
  return $ take n $ randomRs (0, length ls - 1) gen
