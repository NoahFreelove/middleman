module Main (main) where

import Data.Text (unpack)
import Middleman (loadConfig, startServer, version)
import Middleman.Config (renderConfigError)
import Options.Applicative
  ( Parser
  , ParserInfo
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , infoOption
  , long
  , metavar
  , progDesc
  , short
  , strOption
  , (<**>)
  )
import System.Exit (exitFailure)

data Options = Options
  { optConfig :: FilePath
  }

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILE"
          <> help "Path to config JSON file"
      )

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("middleman v" <> unpack version)
    (long "version" <> help "Show version information")

opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> versionOption <**> helper)
    ( fullDesc
        <> progDesc "Configurable proxy server for AI agents"
        <> header ("middleman v" <> unpack version <> " - proxy server for AI agents")
    )

main :: IO ()
main = do
  options <- execParser opts
  result <- loadConfig (optConfig options)
  case result of
    Left err -> do
      putStrLn $ "Error: " <> unpack (renderConfigError err)
      exitFailure
    Right cfg -> startServer cfg
