module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data CliFlags = CliFlags { inputFile :: String
                         , inPlace :: Bool
                         , configFile :: String
                         } deriving (Show, Eq)

-- | The parser for command line arguments, to be used with
-- optparse-applicative
cliParse :: Parser CliFlags
cliParse =
    CliFlags
        <$> strArgument (metavar "INPUT" <> help "The input filename")
        <*> switch
                (  long "in-place"
                <> short 'i'
                <> help
                       "Whether to modify the file in-place (this is a destructive operation"
                )
        <*> strOption
                (long "config" <> metavar "CONFIG" <> help "The config filename"
                )


printFile :: CliFlags -> IO ()
printFile (CliFlags _ filename _) = print filename

main :: IO ()
main = printFile =<< execParser opts
  where
    opts =
        info (cliParse <**> helper)
            $  fullDesc
            <> progDesc "Format a markdown file"
            <> header "mdfmt - a markdown formatter"
