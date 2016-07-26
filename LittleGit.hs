import System.Environment
import System.FilePath
import System.Directory
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
  args <- getArgs
  let dir = head args
  cp dir (dir ++ ".bak")

cp :: FilePath -> FilePath -> IO ()
cp src dest = do
  isFile <- doesFileExist src
  isDir <- doesDirectoryExist src
  case (isFile, isDir) of
    (True, False) -> copyFile src dest
    (False, True) -> do
      allFiles <- getDirectoryContents src
      let files = filter (`notElem` [".", ".."]) allFiles
      let mkDir = createDirectory dest
      let copyFiles = mapM_ (\fp -> cp (src </> fp) (dest </> fp)) files
      mkDir *> copyFiles
    _ -> error ("path does not exist:" ++ src)
