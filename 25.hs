module RandPerm (randPerm) where
import System.Random

randPerm :: [a] -> IO [a]
randPerm ls = 
