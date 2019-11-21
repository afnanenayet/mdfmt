module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           Data.Text.IO                  as TIO
import           Data.Text.Lazy.IO             as TLIO
import           CMark
import           Control.Monad
import           Data.Maybe

data CliFlags = CliFlags { inputFile :: String
                         , inPlace :: Bool
                         , configFile :: Maybe String
                         , textWidth :: Maybe Int
                         } deriving (Show, Eq)

-- | The default column/text width
defaultTextWidth :: Int
defaultTextWidth = 80

-- | The parser for command line arguments, to be used with
-- optparse-applicative
cliParse :: Parser CliFlags
cliParse =
    CliFlags
        <$> strArgument (metavar "INPUT" <> help "The input filename")
        <*> switch
                (long "in-place" <> short 'i' <> help
                    (concat
                        [ "Whether to modify the file in-place (this is "
                        , "a destructive operation)"
                        ]
                    )
                )
        <*> (optional $ strOption
                (long "config" <> short 'c' <> metavar "CONFIG" <> help
                    (concat
                        [ "[optional] The filename of a config file. If"
                        , " this is not supplied then the program will use the"
                        , " default configuration values."
                        ]
                    )
                )
            )
        <*> (optional $ option
                auto
                (long "width" <> short 'w' <> metavar "WIDTH" <> help
                    "The desired column width of the formatted document"
                )
            )

-- | The options that we use with commonmark
cmarkOptions :: [CMarkOption]
cmarkOptions = [optUnsafe, optNormalize]

main :: IO ()
main = do
    args         <- execParser opts
    fileContents <- TIO.readFile (inputFile args)
    let nodes     = commonmarkToNode cmarkOptions fileContents
    let width     = fromMaybe defaultTextWidth (textWidth args)
    let formatted = nodeToCommonmark cmarkOptions (Just width) nodes
    TIO.putStr formatted
  where
    opts =
        info (cliParse <**> helper)
            $  fullDesc
            <> progDesc "Format markdown files"
            <> header "mdfmt - a markdown formatter"
