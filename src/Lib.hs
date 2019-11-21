module Lib
    ( wordWrap
    , WordWrapError
    )
where

import qualified Data.Text                     as T

-- | An error representing the possible errors that arise from word wrapping
data WordWrapError a = InvalidColumnWidth Int

instance Show a => Show (WordWrapError a) where
    show (InvalidColumnWidth a) =
        mconcat ["The supplied column width ", show a, " is invalid"]

-- | The safe variant of `tail`. This method will return every element after
-- the head of the list. If the list is empty, then this will return an empty
-- list. If the list is comprised of one element, this will also return an
-- empty list.
optionalTail :: [a] -> [a]
optionalTail []       = []
optionalTail [x     ] = []
optionalTail (x : xs) = xs

-- | Word wrap some text to the desired column length
wordWrap :: T.Text -> Int -> Either (WordWrapError a) T.Text
wordWrap input width | width < 1 = Left $ InvalidColumnWidth width
                     | otherwise = Right $ T.unlines linesAsList
  where
    linesAsList = (gobble (T.words input) [] :: [T.Text])
    gobble :: [T.Text] -> [T.Text] -> [T.Text]
    gobble []             _  = []
    gobble [word        ] _  = [word]
    -- gobble [word        ] []  = [word] --< this is the line that failed, changing the [] to a _ fixed our problems
    gobble (word : words) [] = gobble words [word]
    gobble (word : words) allLines | charsLeft > 0 = gobble words newLines
                                   | otherwise = gobble words (word : allLines)
      where
        line      = head allLines
        myLines   = optionalTail allLines
        charsLeft = width - (T.length line)
        newLines  = mconcat [line, " "] : myLines
