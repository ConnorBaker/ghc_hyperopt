module Main where

import Control.Monad
import GHC.Records
import Options.Applicative
import Prelude
import Data.String ( IsString(fromString) ) 


data Sample = Sample
  { name :: String,
    quiet :: Bool,
    enthusiasm :: Int
  }

sample :: Parser Sample
sample =
  Sample
    <$> strOption
      ( long "hello"
          <> metavar "NAME"
          <> help "Name for the greeting"
      )
    <*> switch
      ( long "quiet"
          <> short 'q'
          <> help "Whether to be quiet"
      )
    <*> option
      auto
      ( long "enthusiasm"
          <> help "How enthusiastically to greet"
          <> showDefault
          <> value 1
          <> metavar "INT"
      )

main :: IO ()
main = greet =<< execParser opts
  where
    opts :: ParserInfo Sample
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for NAME"
            <> header "hello - a test for optparse-applicative"
        )

greet :: Sample -> IO ()
greet s = putStrLn $ "Hello, " ++ s.name ++ replicate s.enthusiasm '!'