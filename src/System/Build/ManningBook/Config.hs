module System.Build.ManningBook.Config where

import Control.Monad.Identity hiding (sequence)
import Control.Monad.Trans
import Control.Applicative
import Codec.Archive.Zip
import System.Command
import System.FilePath

data Config =
  Config {
    src :: FilePath
  , livebook :: String
  , dependencyDirectory :: FilePath
  , aavalidator_version :: String
  , aamakepdf_version :: String
  , zipOptions :: [ZipOption]
  , java :: [String] -> IO ExitCode
  }

defaultConfig ::
  Config
defaultConfig =
  Config {
    src = "src" </> "book.xml"
  , livebook = "http://livebook.manning.com/"
  , dependencyDirectory = "lib"
  , aavalidator_version = "14.2"
  , aamakepdf_version = "18.4"
  , zipOptions = [OptVerbose]
  , java = rawSystem "java"
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

newtype ConfigerT f a =
  ConfigerT {
    (==>>>) :: Config -> f a
  }

type Configer a =
  ConfigerT Identity a

configer ::
  (Config -> a)
  -> Configer a
configer f =
  ConfigerT (Identity . f)

(=>>>) ::
  Configer a
  -> Config
  -> a
(=>>>) g =
  runIdentity . (==>>>) g

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
      k c >>= \a -> f a ==>>> c

instance MonadTrans ConfigerT where
  lift =
    ConfigerT . const

