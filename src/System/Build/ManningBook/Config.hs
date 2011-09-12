module System.Build.ManningBook.Config where

import Control.Monad.Identity
import Data.Enumerator
import Data.Enumerator.Binary
import Network.HTTP.Enumerator
import System.IO
import System.FilePath
import System.FilePath.FilePather
import System.Directory
import Control.Applicative
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as L

data Config =
  Config {
    livebook :: String
  , dependencyDirectory :: FilePath
  , aavalidator_version :: String
  , aamakepdf_version :: String
  }

defaultConfig ::
  Config
defaultConfig =
  Config {
    livebook = "http://livebook.manning.com/"
  , dependencyDirectory = "lib"
  , aavalidator_version = "14.2"
  , aamakepdf_version = "18.4"
  }

aavalidator ::
  Configer String
aavalidator =
  configer $ \c -> livebook c ++ "AAValidatorv" ++ aavalidator_version c ++ ".zip"

aamakepdf ::
  Configer String
aamakepdf =
  configer $ \c -> livebook c ++ "AAMakePDFv" ++ aamakepdf_version c ++ ".zip"

docbookindexer ::
  Configer String
docbookindexer =
  configer $ \c -> livebook c ++ "09_docbookIndexer.zip"

pdfmaker ::
  Configer String
pdfmaker =
  configer $ \c -> livebook c ++ "static/pdfmaker.xsl"

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
  do x <- downloadDependencyIfNotAlready "AAValidator.zip" aamakepdf
     _ <- unless x $ ConfigerT $ \c ->
                       let d = dependencyDirectory c </> "AAValidator"
                       in do createDirectoryIfMissing True d
                             indir d (\z -> L.readFile (z </> dependencyDirectory c </> "AAValidator.zip") >>= extractFilesFromArchive [OptVerbose] . toArchive)
     return x

newtype ConfigerT f a =
  ConfigerT {
    runConfigerT :: Config -> f a
  }

type Configer a =
  ConfigerT Identity a

configer ::
  (Config -> a)
  -> Configer a
configer f =
  ConfigerT (Identity . f)

runConfiger ::
  Configer a
  -> Config
  -> a
runConfiger g =
  runIdentity . runConfigerT g

instance Functor f => Functor (ConfigerT f) where
  fmap f (ConfigerT z) =
    ConfigerT $ fmap f . z

instance Applicative f => Applicative (ConfigerT f) where
  pure =
    ConfigerT . pure . pure
  ConfigerT f <*> ConfigerT a =
    ConfigerT (liftA2 (<*>) f a)

instance Monad f => Monad (ConfigerT f) where
  return =
    ConfigerT . return . return
  ConfigerT k >>= f =
    ConfigerT $ \c ->
      k c >>= \a -> runConfigerT (f a) c


