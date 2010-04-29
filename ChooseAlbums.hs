import System.Environment (getArgs)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM)
import System.Random (randomRs, mkStdGen)
import Data.Char (digitToInt)

asInteger :: String -> Int
asInteger s | null s = 0
            | '.' `elem` s = error "Not an integer."
            | head s == '-' = (-1) * foldl intify 0 (tail s)
            | otherwise = foldl intify 0 s
            where intify c c' = 10 * c + digitToInt c'

getRandoms :: Int -> Int -> [Int]
getRandoms seed x = (randomRs (0, x) $ mkStdGen seed) :: [Int]

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

chooseAlbums :: Int -> Int -> FilePath -> IO ()
chooseAlbums seed n dir = do
  alphabet' <- getDirectoryContents dir
  let alphabet = map (dir </>) . filter (`notElem` [".", ".."]) $ alphabet'
  betabet <- getNextLevel alphabet
  gammabet <- getNextLevel betabet
  let choices = take n $ getRandoms seed (length gammabet - 1)
  let deltabet = foldr (\x y -> (gammabet !! x) : y) [] choices
--  mapM_ putStrLn gammabet
--  mapM_ putStrLn (map show choices)
  mapM_ putStrLn deltabet

main :: IO ()
main = do
  args <- getArgs
  let seed = asInteger $ args !! 0
  let amount = asInteger $ args !! 1
  chooseAlbums seed amount $ args !! 2
