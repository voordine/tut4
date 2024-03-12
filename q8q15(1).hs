import System.IO
import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.List
{-# LANGUAGE BangPatterns #-}


--question 8
readVecSpace = do
    file <- openFile "sample_vecspace_an.txt" ReadMode
    contents <- hGetContents file
    let lns = [words l | l <- lines contents]
    let !mp = Map.fromList $ map keyVal lns
    hClose file
    pure mp
    where
        keyVal :: [String] -> (String, [Float])
        keyVal [] = undefined
        keyVal (h:ts) = (h, map read ts)
{- In order to remove type IO, write in terminal
 vecspace <- readVecSpace
 this might take a while
 You can now use vecspace to test your other functions in the following questions
	-}	

		
-- question 15				
anDataIO = do
  compositionFile <- openFile "sample_composition_an.txt" ReadMode
  cinput <- hGetContents compositionFile
  let clines = [words cline | cline <- lines cinput]
      phrases = [(phrase !! 1, phrase !! 2) | phrase <- clines]
  pure phrases
 
 {- In order to remove type IO, write in terminal
 anData <- anDataIO
 this might take a while
 You can now use anData to test your other functions in the following questions
	-}	

  
  
 
  
	