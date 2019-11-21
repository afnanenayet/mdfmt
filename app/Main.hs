module Main where

import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import qualified Data.Text.IO                  as TIO
import qualified Data.Text                     as T
import           CMarkGFM
import           Data.Maybe

data CliFlags = CliFlags { inputFile :: String
                         , inPlace :: Bool
                         , configFile :: Maybe String
                         , textWidth :: Maybe Int
                         , outputFile :: Maybe String
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
                        [ "Whether to modify the file in-place (this is"
                        , " a destructive operation) and can be used"
                        , " concurrently with `--out OUT`"
                        ]
                    )
                )
        <*> optional
                (strOption
                    (long "config" <> short 'c' <> metavar "CONFIG" <> help
                        (concat
                            [ "The filename of a config file. If"
                            , " this is not supplied then the program will use the"
                            , " default configuration values."
                            ]
                        )
                    )
                )
        <*> optional
                (option
                    auto
                    (long "width" <> short 'w' <> metavar "WIDTH" <> help
                        "The desired column width of the formatted document"
                    )
                )
        <*> optional
                (strOption
                    (long "out" <> short 'o' <> metavar "OUT" <> help
                        ("The path write the formatted output to. This can be "
                        ++ "used concurrently with the `-i` flag."
                        )
                    )
                )

-- | The options that we use with commonmark
cmarkOptions :: [CMarkOption]
cmarkOptions = [optUnsafe]

-- | GFM extensions that are enabled for the parser. We enable all of them to
-- stay compliant with Github.
--
-- TODO: with enough interest we can expose this as a command line option
defaultCmarkExtensions :: [CMarkExtension]
defaultCmarkExtensions = [extTable, extAutolink, extTable, extTagfilter]

-- | Export the formatted contents of a file to the appropriate place given the
-- command line arguments from the user. The fallback/default action is to
-- write to STDOUT.
exportOutput :: CliFlags -> T.Text -> IO ()
exportOutput (CliFlags inFile True _ _ Nothing) contents =
    TIO.writeFile inFile contents
exportOutput (CliFlags _ False _ _ (Just outFile)) contents =
    TIO.writeFile outFile contents
exportOutput (CliFlags inFile True _ _ (Just outFile)) contents =
    TIO.writeFile outFile contents >> TIO.writeFile inFile contents
exportOutput _ contents = TIO.putStr contents

main :: IO ()
main = do
    args         <- execParser opts
    fileContents <- TIO.readFile (inputFile args)
    let nodes =
            commonmarkToNode cmarkOptions defaultCmarkExtensions fileContents
    let width     = fromMaybe defaultTextWidth (textWidth args)
    let formatted = nodeToCommonmark cmarkOptions (Just width) nodes
    exportOutput args formatted
  where
    opts =
        info (cliParse <**> helper)
            $  fullDesc
            <> progDesc "Format markdown files"
            <> header "mdfmt - a markdown formatter"
