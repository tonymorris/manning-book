module System.Build.ManningBook.Build where

import Prelude hiding (mapM_)
import System.Build.ManningBook.Config
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L
import System.FilePath.FilePather
import qualified System.FilePath.FilePather as P
import System.FilePath
import System.Directory
import System.Command
import Control.Monad hiding (mapM_)
import Control.Monad.Writer hiding (mapM_)
import System.IO
import Network.HTTP.Enumerator
import Data.Enumerator.Binary hiding (mapM_)
import Data.Enumerator hiding (sequence, length)
import Data.List
import Data.Foldable

downloadDependencyIfNotAlready ::
  FilePath
  -> Configer String
  -> ConfIO Bool
downloadDependencyIfNotAlready f g =
  ConfigerT $ \c ->
    let d = dependencyDirectory c
        p = d </> f
    in mkdir d >>
         (WriterT $
           do e <- doesFileExist p
              if e
                then
                  return (True, return $ "Dependency " ++ p ++ " already exists")
                else
                  withFile p WriteMode $ \h ->
                    do let u = g =>>> c
                       r <- parseUrl u
                       withManager $ run_ . httpRedirect r (\_ _ -> iterHandle h)
                       return (False, return $ "Retrieved dependency " ++ p ++ " from " ++ show u))

mkdir ::
  FilePath
  -> Log IO ()
mkdir d =
  do e <- doesDirectoryExist d ++> ("Making directory " ++ d)
     if e
       then
         return () ++> ("Directory " ++ d ++ " already exists")
       else
         do createDirectoryIfMissing True d ++> ("Creating directory " ++ d)
            return () ++> ("Created directory " ++ d)

aavalidatorDownload ::
  ConfIO Bool
aavalidatorDownload =
  do x <- downloadDependencyIfNotAlready "AAValidator.zip" aavalidator
     _ <- unless x . ConfigerT $ \c ->
                       let d = dependencyDirectory c </> "AAValidator"
                       in do _ <- mkdir d
                             indir d (\z -> L.readFile (z </> dependencyDirectory c </> "AAValidator.zip") >>=
                                            extractFilesFromArchive (zipOptions c) . toArchive) ++> ("Extract files from archive in directory " ++ d)
     return x

aamakepdfDownload ::
  ConfIO Bool
aamakepdfDownload =
  do x <- downloadDependencyIfNotAlready "AAMakePDF.zip" aamakepdf
     _ <- unless x . ConfigerT $ \c ->
                       let d = dependencyDirectory c </> "AAMakePDF"
                       in do _ <- mkdir d
                             indir d (\z -> L.readFile (z </> dependencyDirectory c </> "AAMakePDF.zip") >>=
                                            extractFilesFromArchive (zipOptions c) . toArchive) ++> ("Extract files from archive in directory " ++ d)
     return x

docbookindexerDownload ::
  ConfIO Bool
docbookindexerDownload =
  do x <- downloadDependencyIfNotAlready "docbookIndexer.zip" docbookindexer
     _ <- unless x . ConfigerT $ \c ->
                       let lib = dependencyDirectory c
                       in indir lib (\z -> L.readFile (z </> dependencyDirectory c </> "docbookIndexer.zip") >>=
                                           extractFilesFromArchive (zipOptions c) . toArchive) ++> ("Extract files from archive in directory " ++ lib)
     return x

pdfmakerDownload ::
  ConfIO Bool
pdfmakerDownload =
  downloadDependencyIfNotAlready "pdfmaker.xsl" pdfmaker

allDownload ::
  ConfIO [Bool]
allDownload =
  sequence [
    aavalidatorDownload
  , aamakepdfDownload
  , docbookindexerDownload
  , pdfmakerDownload
  ]

mkDistDir ::
  ConfIO ()
mkDistDir =
  ConfigerT $
    mkdir . takeDirectory . distFile

pdf ::
  CLog IO ([Bool], ExitCode)
pdf =
  do h <- allDownload
     _ <- mkDistDir
     i <- ConfigerT $ \c ->
            indir (dependencyDirectory c </> "AAMakePDF")
                  (\d -> system $ unwords [
                                            java c
                                          , "-Djava.ext.dirs=lib"
                                          , "AAPDFMaker"
                                          , d </> src c
                                          , d </> distFile c
                                          ]) ++> "Generate PDF"
     _ <- ConfigerT $ \c ->
            let junk = src c ++ ".temp.xml"
            in ((doesFileExist junk >>= \k -> k `when` removeFile junk) ++> ("Removing junk file " ++ junk))
     return (h, i)

validate ::
  CLog IO ([Bool], ExitCode)
validate =
  do h <- allDownload
     i <- ConfigerT $ \c ->
            indir (dependencyDirectory c </> "AAValidator")
                  (\d -> do g <- getCurrentDirectory
                            j <- jarFiles g
                            system $ unwords [
                                               java c
                                             , "-classpath"
                                             , intercalate [searchPathSeparator] j
                                             , "AAValidator"
                                             , d </> src c
                                             ]) ++> "Validate"
     _ <- ConfigerT $ \c ->
            let junk = takeDirectory (src c) </> "temp.xml"
            in ((doesFileExist junk >>= \k -> k `when` removeFile junk) ++> ("Removing junk file " ++ junk))
     return (h, i)

spellcheckInit ::
  CLog IO ()
spellcheckInit =
  ConfigerT $ \c ->
    let w f = (do e <- doesFileExist f ++> "Initialize spell-checking"
                  if e
                    then
                       return () ++> ("File " ++ f ++ " already exists")
                    else
                      do mkdir . takeDirectory $ f
                         h <- openFile f AppendMode ++> ("Touch file " ++ f)
                         hClose h ++> ("Close file " ++ f))
    in w (sgmlSkipFile c) >> w (addWordsFile c)

spellcheck ::
  CLog IO [Bool]
spellcheck =
  do spellcheckInit
     h <- allDownload
     ConfigerT $ \c ->
       (do x <- xmlFiles (src c)
           s <- readFile (sgmlSkipFile c)
           t <- canonicalizePath (addWordsFile c)
           mapM_ (\z -> system . unwords $ [
                                             aspell c
                                           , "--dont-backup"
                                           , "--master=" ++ masterDictionary c
                                           , "--encoding=" ++ encoding c
                                           , "--mode=sgml"
                                           , "-p"
                                           , t
                                           , "-c"
                                           , z
                                           ] ++  (("--add-sgml-skip=" ++) `fmap` words s)) x) ++> "Spellcheck"
     return h

spellcheckNoninteractive ::
  CLog IO ([Bool], ExitCode)
spellcheckNoninteractive =
  do spellcheckInit
     h <- allDownload
     x <- ConfigerT $ \c ->
       (mkdir . takeDirectory . spellingErrorsFile $ c) >>
       (do x <- xmlFiles (src c)
           s <- readFile (sgmlSkipFile c)
           t <- canonicalizePath (addWordsFile c)
           (system . unwords $ [
                                 cat c
                               , intercalate " " x
                               , "|"
                               , aspell c
                               , "--dont-backup"
                               , "--master=" ++ masterDictionary c
                               , "--encoding=" ++ encoding c
                               , "--mode=sgml"
                               , "-p"
                               , t
                               , "list"
                               , ">"
                               , spellingErrorsFile c
                               ] ++  (("--add-sgml-skip=" ++) `fmap` words s)) ->>
             do e <- readFile (spellingErrorsFile c)
                let n = length . lines $ e
                hPutStrLn stderr ("(" ++ show n ++ ") spelling error" ++ if n == 1 then [] else "s")
                hPutStrLn stderr e
                return $ if n == 0
                           then success
                           else exitCode 12) ++> "Spellcheck non-interactive"
     return (h, x)

jarFiles ::
  FilePath
  -> IO [FilePath]
jarFiles =
  P.find always (extensionEq "jar")

xmlFiles ::
  FilePath
  -> IO [FilePath]
xmlFiles =
  P.find always (extensionEq "xml")


printLog ::
  Foldable f =>
  f String
  -> ConfigerT IO ()
printLog u =
  ConfigerT $ \c ->
    logging c `when` mapM_ (\w -> putStrLn ("[LOG] " ++ w)) u
