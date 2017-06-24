{- | This is the third assignment for IB016, semester spring 2015.
  Name: Jan Skrasek
  UID: 373816



  ================ POZNAMKA K ULOZE ===================

    V zadaní jest:
    $ ../du first
    1024      first/second
    1524      first

    $ ../du -h first
    1.0 MiB   first/second
    1.4 MiB   first

    $ ../du --si first
    1.1 MB    first/second
    1.5 MB    first

    Jenze pokud dobre pocitam, tak
    1024/1024 = 1
    1524/1024 = 1,488 -> floor needed for 1,4
    1024/1000 = 1,024 -> ceil needed for 1,1
    1524/1000 = 1,524 -> floor|round needed for 1,5

    Otestoval jsem to linuxovym du, a to, dle me, provadi vzdy ceil zaokrouhleni.
    S dovolenim tedy pouzivam ceil.


    Dale: vsechny hlintovy warning jsem videl, ale zapis "foo $ bar $ baz" mi prijde
    prehlednejsi nez "foo . bar $ (baz)", respektive dalsi variace.
 -}

module Main ( main ) where

import Data.Monoid
import Data.Maybe
import Control.Exception ( handle, IOException )
import Control.Monad
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import Text.Printf

data Config = Config
    { units   :: Last Integer
    , humanReadable :: Last Bool
    , includeFiles :: Last Bool
    , maxDepth :: Last Int
    , grandTotal :: Last Bool
    , showHelp :: Last Bool
    , dirs :: [String]
    } deriving (Eq, Show)

type DuEntry = (FilePath, Bool, Int, Integer)
type DuReadableEntry = (FilePath, Bool, Int, String)

instance Monoid Config where
    mempty = emptyConfig
    c1 `mappend` c2 =
        Config (units c1 `mappend` units c2)
               (humanReadable c1 `mappend` humanReadable c2)
               (includeFiles c1 `mappend` includeFiles c2)
               (maxDepth c1 `mappend` maxDepth c2)
               (grandTotal c1 `mappend` grandTotal c2)
               (showHelp c1 `mappend` showHelp c2)
               (dirs c1 `mappend` dirs c2)

emptyConfig :: Config
emptyConfig = Config mempty mempty mempty mempty mempty mempty mempty

defaultConfig :: Config
defaultConfig = Config (Last (Just 1024)) (Last (Just False)) (Last (Just False)) mempty (Last (Just False)) (Last (Just False)) mempty

configTable :: [ (String, String -> Config) ]
configTable = [ ("--all",            \_ -> emptyConfig { includeFiles = Last (Just True) } )
              , ("-a",               \_ -> emptyConfig { includeFiles = Last (Just True) } )
              , ("--human-readable", \_ -> emptyConfig { humanReadable = Last (Just True) } )
              , ("-h",               \_ -> emptyConfig { humanReadable = Last (Just True) } )
              , ("--si",             \_ -> emptyConfig { humanReadable = Last (Just True), units = Last (Just 1000) } )
              , ("--summarize",      \_ -> emptyConfig { maxDepth = Last (Just 0) } )
              , ("-s",               \_ -> emptyConfig { maxDepth = Last (Just 0) } )
              , ("--max-depth",      \p -> emptyConfig { maxDepth = Last (Just $ read $ tail $ dropWhile (/= '=') p) })
              , ("-d",               \p -> emptyConfig { maxDepth = Last (Just $ read $ tail $ tail $ p) })
              , ("--total",          \_ -> emptyConfig { grandTotal = Last (Just True) } )
              , ("-c",               \_ -> emptyConfig { grandTotal = Last (Just True) } )
              , ("--help",           \_ -> emptyConfig { showHelp = Last (Just True) } )
              ]

processArg :: String -> Config
processArg arg = let opt = paramName arg in
    case lookup opt configTable of
        Nothing -> emptyConfig { dirs = [arg] }
        Just f -> f arg

paramName :: String -> String
paramName p = case head p of
                '-' -> case head $ tail p of
                            '-' -> takeWhile (/= '=') p
                            _   -> ['-', head $ tail p]
                _   -> ""

parse :: IO Config
parse = do
            args <- getArgs
            return . mconcat $ (defaultConfig : map processArg args)

failure :: IOException -> IO ()
failure ex = hPutStrLn stderr $ "Fatal error: " ++ show ex

failure2 :: IOException -> IO [DuEntry]
failure2 ex = do
                hPutStrLn stderr $ "Fatal error: " ++ show ex
                return []

main :: IO ()
main = handle failure $ do
    config <- parse
    case getLast $ showHelp config of
        Just False      -> du config
        Just True       -> help
        Nothing         -> help

help :: IO ()
help = do
    name <- getProgName
    hPutStrLn stderr $ unlines [
            "usage: " ++ name ++ " [options] [files-or-directories]",
            "  -a, --all             write counts for all files, not just directories",
            "  -h, --human-readable  print sizes in human readable format (e.g., 1KiB 234MiB 2GiB)",
            "      --si              like -h, but use powers of 1000 not 1024 (e.g., 1KB, 245MB, 2.1GB)",
            "  -s, --summarize       display only a total for each argument",
            "  -d, --max-depth       print the total for a directory (or file, with --all)",
            "                        only if it is N or fewer levels below the command line",
            "                        argument; --max-depth=0 is the same as --summarize",
            "  -c, --total           produce a grand total",
            "      --help            display help and exit"
        ]

du :: Config -> IO ()
du config = do
        currentDir <- getCurrentDirectory
        let targets = if null targets then ["."] else targets where targets = dirs config

        duEntries <- mapM (\dir -> getFileSizes currentDir dir 0) targets

        let entries = applyReadable config
                    . applyAll config
                    . applyMaxDepth config
                    $ applyGrandTotal config (concat duEntries)

        mapM_ (putStrLn . (\(file,_,depth,size) -> size ++ "   " ++ file)) entries
        return ()


applyGrandTotal :: Config -> [DuEntry] -> [DuEntry]
applyGrandTotal c e
    | getLast (grandTotal c) == Just True  = addDirectorySum "total" e 0
    | otherwise                            = e

applyAll :: Config -> [DuEntry] -> [DuEntry]
applyAll c e
    | getLast (includeFiles c) == Just True = e
    | otherwise                             = filter (\(_, isFile, _, _) -> not isFile) e

applyMaxDepth :: Config -> [DuEntry] -> [DuEntry]
applyMaxDepth c e = let max_depth = getLast $ maxDepth c in
        case max_depth of
            Just n  -> filter (\(_, _, depth, _) -> depth <= n) e
            Nothing -> e

applyReadable :: Config -> [DuEntry] -> [DuReadableEntry]
applyReadable c e
    | getLast (humanReadable c) == Just True = map (\(file,isFile,depth,size) -> (file,isFile,depth,readableSize divisor size)) e
    | otherwise                              = map (\(file,isFile,depth,size) -> (file,isFile,depth,show $ size `div` 1024)) e
    where divisor = fromInteger $ fromJust $ getLast $ units c

getFileSizes :: FilePath -> FilePath -> Int -> IO [DuEntry]
getFileSizes root prefix depth = handle failure2 $ do
                let from = root </> prefix
                isDirectory <- doesDirectoryExist from
                if not isDirectory
                    then do
                        fileSize <- getFileSize from
                        return [(prefix, True, depth, fileSize), (prefix, False, depth, fileSize)]
                    else do
                        paths <- getDirectoryContents from;
                        let properPaths = filter (`notElem` [".", ".."]) paths
                        filePaths <- forM properPaths $ \name -> do
                            let path = from </> name
                            let fileName = prefix </> name
                            isDirectory <- doesDirectoryExist path
                            if isDirectory
                                then getFileSizes root fileName (depth + 1)
                                else do
                                    fileSize <- getFileSize path
                                    return [(fileName, True, depth, fileSize)]
                        let allFiles = concat filePaths
                        return $ addDirectorySum prefix allFiles depth

getFileSize :: String -> IO Integer
getFileSize path = withFile path ReadMode hFileSize


addDirectorySum :: FilePath -> [DuEntry] -> Int -> [DuEntry]
addDirectorySum from allFiles depth = allFiles++[(from, False, depth, total)]
    where total = sum (map (\(_, isFile, _, size) -> if isFile then size else 0) allFiles)



readableSize :: Float -> Integer -> String
readableSize base size = stringify . head $ dropWhile (\(x,_) -> x >= base) (zip powers sufixes)
        where sufixes   = if base == 1000.0 then ["B", "kB", "MB", "GB", "TB", "PB"] else ["B", "KiB", "MiB", "GiB", "TiB", "PiB"]
              powers    = [fromInteger (ceiling ((fromInteger size / (base^^power)) * 10)) / 10 | power <- [0..5]]
              stringify (size, suffix) = (printf "%.2f" size) ++ " " ++ suffix


formatNumber :: String -> Float -> String
formatNumber suffix = case suffix of
                        "kB" -> printf "%.0f"
                        "KiB" -> printf "%.0f"
                        _ -> printf "%.1f"
