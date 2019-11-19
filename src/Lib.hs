module Lib
    ( wordWrap
    , WordWrapError
    )
where

import qualified Data.Text                     as T

-- | An error representing what can go wrong when performing a word wrap
data WordWrapError a = InvalidColumnWidth Int

-- | Word wrap some text to the desired column length
wordWrap :: T.Text -> Int -> Either (WordWrapError a) T.Text
wordWrap input width | width < 1 = Left $ InvalidColumnWidth width
                     | otherwise = Right $ T.unlines wrappedLines
  where
    wrappedLines = map T.unwords $ gobble 0 [] $ T.words input
    gobble :: Int -> [T.Text] -> [T.Text] -> [[T.Text]]
    gobble _ acc [] = [reverse acc]
    gobble k acc ws@(w : rest)
        | 1 >= width     = reverse acc : [w] : gobble 0 [] rest
        | k + 1 >= width = reverse acc : gobble 0 [] ws
        | otherwise      = gobble (k + l + 1) (w : acc) rest
        where l = T.length w
