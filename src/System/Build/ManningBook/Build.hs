module System.Build.ManningBook.Build where

import System.Build.ManningBook.Config
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L
import System.FilePath.FilePather
import System.FilePath
import System.Directory
import Control.Monad
import System.IO
import Network.HTTP.Enumerator
import Data.Enumerator.Binary
import Data.Enumerator hiding (sequence)

downloadDependencyIfNotAlready ::
  FilePath
  -> Configer String
  -> ConfigerT IO Bool
downloadDependencyIfNotAlready f g =
  ConfigerT $ \c ->
    let d = dependencyDirectory c
        p = d </> f
    in do e <- doesFileExist p
          if e
            then
              return True
            else
              do createDirectoryIfMissing True d
                 withFile p WriteMode $ \h ->
                   do r <- parseUrl (runConfiger g c)
                      withManager $ run_ . httpRedirect r (\_ _ -> iterHandle h)
                      return False

aavalidatorDownload ::
  ConfigerT IO Bool
aavalidatorDownload =
  do x <- downloadDependencyIfNotAlready "AAValidator.zip" aavalidator
     _ <- unless x $ ConfigerT $ \c ->
                       let d = dependencyDirectory c </> "AAValidator"
                       in do createDirectoryIfMissing True d
                             indir d (\z -> L.readFile (z </> dependencyDirectory c </> "AAValidator.zip") >>= extractFilesFromArchive [OptVerbose] . toArchive)
     return x

aamakepdfDownload ::
  ConfigerT IO Bool
aamakepdfDownload =
  do x <- downloadDependencyIfNotAlready "AAMakePDF.zip" aamakepdf
     _ <- unless x $ ConfigerT $ \c ->
                       let d = dependencyDirectory c </> "AAMakePDF"
                       in do createDirectoryIfMissing True d
                             indir d (\z -> L.readFile (z </> dependencyDirectory c </> "AAMakePDF.zip") >>= extractFilesFromArchive [OptVerbose] . toArchive)
     return x

docbookindexerDownload ::
  ConfigerT IO Bool
docbookindexerDownload =
  do x <- downloadDependencyIfNotAlready "docbookIndexer.zip" docbookindexer
     _ <- unless x $ ConfigerT $ \c ->
                       let lib = dependencyDirectory c
                           d = lib </> "docbookIndexer"
                       in do createDirectoryIfMissing True d
                             indir lib (\z -> L.readFile (z </> dependencyDirectory c </> "docbookIndexer.zip") >>= extractFilesFromArchive [OptVerbose] . toArchive)
     return x

pdfmakerDownload ::
  ConfigerT IO Bool
pdfmakerDownload =
  downloadDependencyIfNotAlready "pdfmaker.xsl" pdfmaker

allDownload ::
  ConfigerT IO [Bool]
allDownload =
  sequence [
    aavalidatorDownload
  , aamakepdfDownload
  , docbookindexerDownload
  , pdfmakerDownload
  ]
