{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}


module Main where


import           Control.Monad
import qualified Data.Text                 as T
import qualified Filesystem                as FS
import           Options.Applicative
import           Options.Applicative.Types
import           Prelude                   hiding (FilePath)
import           Shelly

default (T.Text)


dvdDir :: IO FilePath
dvdDir = fmap (</> ("dvds" :: FilePath)) FS.getHomeDirectory

newtype BaseName = BaseName { unBaseName :: T.Text }
                   deriving (Show, Eq)

newtype Disk = Disk { unDisk :: Int }
               deriving (Eq)

instance Show Disk where
    show (Disk n) = "/dev/disk" ++ show n


main :: IO ()
main = execParser opts >>= \(BaseName baseName) ->
    shelly $ verbosely $ do

    diskutil_ "list" []
    disk   <- readDisk "Enter the disk #: "
    output <- (<.> "iso") . (</> fromText baseName) <$> liftIO dvdDir
    let disk'   = T.pack $ show disk
        output' = toTextIgnore output
    echo $ "ripping " <> disk' <> " to " <> output'

    unmount_ disk
    errExit False $
        dd_ disk output
    retcode <- lastExitCode

    notify_ "rip-dvd" $  "Done ripping " <> disk'
                      <> " => " <> output'
                      <> ": return code = " <> T.pack (show retcode)
    eject_ disk


diskutil_ :: T.Text -> [T.Text] -> Sh ()
diskutil_ = command1_ "diskutil" []

unmount_ :: Disk -> Sh ()
unmount_ = diskutil_ "unmount" . pure . T.pack . show

eject_ :: Disk -> Sh ()
eject_ = diskutil_ "eject" . pure . T.pack . show

dd_ :: Disk -> FilePath -> Sh ()
dd_ from to = run_ "dd" ["if=" <> T.pack (show from), "of=" <> toTextIgnore to]

notify_ :: T.Text -> T.Text -> Sh ()
notify_ title msg = run_ "terminal-notifier" [ "-title",   title
                                             , "-message", msg
                                             ]

readDisk :: T.Text -> Sh Disk
readDisk = const (Disk <$> liftIO readLn) <=< echo_n


opts' :: Parser BaseName
opts' =   BaseName
      <$> argument readText (  metavar "BASENAME"
                            <> help "Rips to dvds/[BASENAME].iso.")

opts :: ParserInfo BaseName
opts = info (helper <*> opts')
            (  fullDesc
            <> progDesc "Rips a DVD to an ISO."
            <> header "rip-dvd - rips a DVD to an ISO.")

readText :: ReadM T.Text
readText = fmap T.pack readerAsk
