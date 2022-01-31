module Main where

import System.IO ( hIsEOF, openFile, hGetLine, Handle, IOMode(ReadMode) )
import System.Directory ( doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents )

main :: IO ()
getFiles :: FilePath -> [FilePath] -> IO [FilePath]
countLine :: Handle -> IO Int
countLines :: [FilePath] -> IO Int

getFiles _ [a, b] = return []
getFiles prefix (h : t) = do
  if (or [h == "node_modules", h == "dist-newstyle"]) then
    return []
  else do
    let dirName = if prefix == [] then h else prefix ++ "/" ++ h
    let fileName = if prefix == [] then h else prefix ++ "/" ++ h

    fileExists <- doesFileExist fileName
    dirExists <- doesDirectoryExist dirName
    rest <- getFiles prefix t

    if fileExists then
      return (fileName : rest)
    else if dirExists then do
      newDirContents <- getDirectoryContents dirName
      dirFiles <- getFiles dirName newDirContents

      return (dirFiles ++ rest)
    else
      return rest

countLine handle = do
  isEOF <- hIsEOF handle

  if isEOF then
    return 0
  else do
    hGetLine handle
    nextNumber <- countLine handle

    return (1 + nextNumber)

countLines [] = return 0
countLines (h : t) = do
  handle <- openFile h ReadMode
  lines <- countLine handle
  nextNumber <- countLines t

  return (lines + nextNumber)

main = do
  curDir <- getCurrentDirectory
  dirContents <- getDirectoryContents curDir
  files <- getFiles curDir dirContents
  lines <- countLines files

  putStrLn "SLOC v1 built with Haskell.\n"
  putStrLn ("Your Project Total of Lines: " ++ show lines)