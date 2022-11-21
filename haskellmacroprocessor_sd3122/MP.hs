module MP where

import Control.Monad.Error (Error (strMsg))
import Distribution.Compat.Lens (_1)
import System.Environment

type FileContents = String

type Keyword = String

type KeywordValue = String

type KeywordDefs = [(Keyword, KeywordValue)]

separators :: String
separators =
  " \n\t.,:;!\"\'()<>/\\"

-----------------------------------------------------

lookUp :: String -> [(String, a)] -> [a]
lookUp x s = [b | (a, b) <- s, a == x]

splitText' (result1, result2) lastString separators "" =
  (result1, result2 ++ [lastString])
splitText' (result1, result2) lastString separators (char : text)
  | char `elem` separators =
      splitText' (result1 ++ [char], result2 ++ [lastString]) "" separators text
  | otherwise =
      splitText' (result1, result2) (lastString ++ [char]) separators text

splitText :: [Char] -> String -> (String, [String])
splitText = splitText' ("", []) ""

combine :: String -> [String] -> [String]
combine [] xs = xs
combine chars [] = []
combine (char : chars) (x : xs) = x : [char] : combine chars xs

splitAtFirstSpace :: String -> String -> (String, String)
splitAtFirstSpace xs [] = (xs, "")
splitAtFirstSpace xs (' ' : ys) = (xs, ys)
splitAtFirstSpace xs (y : ys) = splitAtFirstSpace (xs ++ [y]) ys

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs = map (splitAtFirstSpace "")

expand :: FileContents -> FileContents -> FileContents
expand x defs =
  concat $
    combine splitChars $
      map
        ( \word ->
            let results = lookUp word keywordDefs
             in if null results then word else head results
        )
        textWords
  where
    (splitChars, textWords) = splitText separators x
    keywordDefs = getKeywordDefs $ snd $ splitText "\n" defs

-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String
concatWithSeparator :: String -> [String] -> String
concatWithSeparator separator [] = ""
concatWithSeparator separator [x] = x
concatWithSeparator separator (x : xs) =
  x ++ separator ++ concatWithSeparator separator xs

enhancedExpand :: FileContents -> FileContents -> FileContents
enhancedExpand x defs =
  concatWithSeparator "\n-----\n" $ map (expand x) textBlocks
  where
    (splitChars, textBlocks) = splitText "#" defs

-----------------------------------------------------

-- The provided main program which uses your functions to merge a
-- template and source file.
main :: IO ()
main = do
  args <- getArgs
  main' args
  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")
