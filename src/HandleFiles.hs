module HandleFiles
where

import System.Directory
import System.IO.Strict as SS
import System.IO
import Data.String.Utils

eDir x = x ++ "/EuclidMaze/" 

makeTmpDir = getTemporaryDirectory >>= 
             (\x -> createDirectoryIfMissing False (eDir x)) 

rmTmpDir = getTemporaryDirectory >>= 
           (\x -> removeDirectoryRecursive (eDir x))

getFile name = do t <- getTemporaryDirectory 
                  let newLoc = eDir t ++ name
                  file <- SS.run $ do 
                            x <- SS.openBinaryFile ("data/" ++ name) ReadMode
                            SS.hGetContents x
                  SS.run $ do x <- SS.openBinaryFile newLoc WriteMode
                              SS.hPutStr x (replace "RECCA" "a" file)
                              SS.hFlush x 
                  return newLoc

