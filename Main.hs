{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Options.Applicative
import           Options.Applicative.Types
import qualified Data.Text as T


newtype BaseName = BaseName { unBaseName :: T.Text }
                   deriving (Show, Eq)


main :: IO ()
main = print =<< execParser opts


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
