import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM)

getNextLevel :: [FilePath] -> IO [FilePath]
getNextLevel paths = do
  nextLevel <- forM paths $ \path -> do
                       isDir <- doesDirectoryExist path
                       if isDir
                          then do
                            contents <- getDirectoryContents path
                            return $ map (path </>) . filter (`notElem` [".", ".."]) $ contents
                          else return []
  return $ concat nextLevel

chooseAlbums :: FilePath -> IO ()
chooseAlbums dir = do
  alphabet' <- getDirectoryContents dir
  let alphabet = map (dir </>) . filter (`notElem` [".", ".."]) $ alphabet'
  betabet <- getNextLevel alphabet
  gammabet <- getNextLevel betabet
  mapM_ putStrLn gammabet

main :: IO ()
main = do
  args <- getArgs
  chooseAlbums $ head args
