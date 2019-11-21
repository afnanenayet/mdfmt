import           Test.QuickCheck
import qualified Data.Text                     as T
import           Data.Either
import           Lib

main :: IO ()
main = do
    quickCheckWith stdArgs { maxSuccess = 100000 } prop_wordsAreWrapped

printWordWrap :: Show b => Either a b -> IO ()
printWordWrap (Left  _   ) = print "error"
printWordWrap (Right text) = print text

prop_wordsAreWrapped :: [Char] -> Int -> Bool
prop_wordsAreWrapped slowString width
    | width < 1                     = isLeft rawResult
    | length (lines slowString) < 1 = True
    | length (words slowString) < 1 = True
    | otherwise                     = maxLineSize <= expectedLineLength
  where
    input              = T.pack slowString
    rawResult          = wordWrap input width
    result             = fromRight "" rawResult
    wrappedLines       = T.lines result
    maxLineSize        = maximum $ map T.length wrappedLines
    maxWordSize        = maximum $ map T.length $ T.words input
    -- if there is a word that's longer than the given width, then the
    -- line will have to be as long as the longest word (at minimum)
    expectedLineLength = max width (maxWordSize + 1)
