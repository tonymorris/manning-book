module System.Build.ManningBook.Config where

import Control.Monad.Identity hiding (sequence)
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Applicative
import Codec.Archive.Zip
import Data.List
import Data.Sequence
import System.FilePath
import System.Directory

data Config =
  Config {
    src :: FilePath
  , distFile :: FilePath
  , livebook :: String
  , dependencyDirectory :: FilePath
  , aavalidator_version :: String
  , aamakepdf_version :: String
  , zipOptions :: [ZipOption]
  , java :: String
  , aspell :: String
  , sgmlSkipFile :: FilePath
  , masterDictionary :: String
  , encoding :: String
  }  deriving (Eq, Read)

instance Show Config where
  show (Config s t l dd v p z j a sk dict enc) =
    intercalate "\n" [
      "Config {"
    , "  src                 = " ++ show s
    , "  distFile            = " ++ show t
    , ", livebook            = " ++ show l
    , ", dependencyDirectory = " ++ show dd
    , ", aavalidator_version = " ++ show v
    , ", aamakepdf_version   = " ++ show p
    , ", zipOptions          = " ++ show z
    , ", java                = " ++ show j
    , ", aspell              = " ++ show a
    , ", sgmlSkipFile        = " ++ show sk
    , ", masterDictionary    = " ++ show dict
    , ", encoding            = " ++ show enc
    , "}"
    ]

defaultConfig ::
  Config
defaultConfig =
  Config {
    src = "src" </> "book.xml"
  , distFile = "out" </> "book.pdf"
  , livebook = "http://livebook.manning.com/"
  , dependencyDirectory = "lib"
  , aavalidator_version = "14.2"
  , aamakepdf_version = "18.4"
  , zipOptions = [OptVerbose]
  , java = "java"
  , aspell = "aspell"
  , sgmlSkipFile = "etc" </> "add-sgml-skip"
  , masterDictionary = "en_US"
  , encoding = "utf-8"
  }

configFile ::
  FilePath
configFile =
  ".manning-book"

configPath ::
  IO FilePath
configPath =
  fmap (\h -> h </> configFile) getHomeDirectory

readConfig ::
  IO Config
readConfig =
  do c <- configPath
     e <- doesFileExist c
     if e
       then
         do p <- getPermissions c
            if readable p
              then
                do z <- readFile c
                   return (read z)
              else
                return defaultConfig
       else
         return defaultConfig

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

type Log f a =
  WriterT (Seq String) f a

type CLog f a =
  ConfigerT (WriterT (Seq String) f) a

(++>) ::
  Monad f =>
  f a
  -> String
  -> Log f a
a ++> m =
  WriterT $
    do y <- a
       return (y, return m)

type ConfIO a =
  ConfigerT (WriterT (Seq String) IO) a

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

