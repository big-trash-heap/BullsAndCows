module GameCode4
        ( readCode
        , showCode
        , compCode
        , genrCode
        , genrStdIO
        , getResult
        , CodePoint
        , CodeString
        , Bulls
        , Cows
        , Code
        , CodeCompare (..)
        , CodeRead (..)
        , CodeCheck (..)
        ) where

import Data.List (nub, elemIndex, delete)
import Data.Char (isDigit)
import System.Random

codeSize :: Bulls
codeSize = 4

type CodePoint  = Code
type CodeString = String
type Bulls      = Int
type Cows       = Int

newtype Code        = CodeC String 
  deriving (Eq)

newtype CodeCompare = CodeCompare (Bulls, Cows)

data CodeRead  = ErrorCharNumber
               | ErrorCharType
               | ErrorCharUnique
               | Success Code

data CodeCheck = Win
               | Result (Bulls, Cows)
               deriving (Eq, Show)

getResult :: CodeCheck -> (Bulls, Cows)
getResult Win        = (codeSize, 0)
getResult (Result t) = t

instance Show CodeRead where
  show ErrorCharNumber = "ErrorCharNumber"
  show ErrorCharType   = "ErrorCharType"
  show ErrorCharUnique = "ErrorCharUnique"
  show (Success c)     = "Success: " ++ showCode c

readCode :: CodeString -> CodeRead
readCode s
  | length s /= codeSize       = ErrorCharNumber
  | not (all isDigit s)        = ErrorCharType
  | length (nub s) /= codeSize = ErrorCharUnique
  | otherwise                  = Success $ CodeC s

showCode :: Code -> CodeString
showCode (CodeC x) = x

compCode :: CodePoint -> Code -> CodeCheck
compCode p c =
  let CodeCompare r@(b, _) = codeCompare p c in
  if b == codeSize then Win else Result r

genrCode :: StdGen -> (Code, StdGen)
genrCode sg = gen ([], ['0' .. '9']) sg codeSize
  where
    gen (bl, _) g 0 = (CodeC bl, g)
    gen (bl, xs) g i =
      let (a, gn) = randomR (0, length xs - 1) g
          v = xs !! a
      in gen (v : bl, delete v xs) gn (i - 1)

genrStdIO :: IO StdGen
genrStdIO = newStdGen

codeCompare :: CodePoint -> Code -> CodeCompare
codeCompare (CodeC p) (CodeC c) = snd $ foldl foldCode (0, CodeCompare (0, 0)) c
  where foldCode (j, prev@(CodeCompare (x, y))) v =
          let jj = j + 1 in
          case elemIndex v p of
            Nothing -> (jj, prev)
            Just ii -> (,) jj $ if ii == j then CodeCompare (x + 1, y) else CodeCompare (x, y + 1)
