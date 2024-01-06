import qualified Data.Char as Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified System.IO as IO
import Text.Regex.TDFA

splitFile :: String -> [String]
splitFile [] = []
splitFile xs = lines xs

getLeftDigit :: Int -> String -> (Int, Char)
getLeftDigit pos (x : xs)
    | Char.isDigit x = (pos, x)
    | otherwise = getLeftDigit (pos + 1) xs

getFromDigits :: String -> (Char, Char)
getFromDigits xs =
    let getLeftDigit0 = getLeftDigit 0
        (_, leftDigit) = getLeftDigit0 xs
        (_, rightDigit) = getLeftDigit0 $ reverse xs
     in (leftDigit, rightDigit)

getFromDigitsPos :: String -> ((Int, Char), (Int, Char))
getFromDigitsPos xs =
    let getLeftDigit0 = getLeftDigit 0
        (idxLeft, leftDigit) = getLeftDigit0 xs
        (idxRight, rightDigit) = getLeftDigit0 $ reverse xs
     in ((idxLeft, leftDigit), (length xs - idxRight - 1, rightDigit))

getMatches :: String -> String -> [(Int, Char)]
getMatches xs w =
    let matches = getAllMatches (xs =~ w) :: [(Int, Int)]
        numberMapping =
            Map.fromList
                [ ("one", '1')
                , ("two", '2')
                , ("three", '3')
                , ("four", '4')
                , ("five", '5')
                , ("six", '6')
                , ("seven", '7')
                , ("eight", '8')
                , ("nine", '9')
                ]
        number = fromJust (Map.lookup w numberMapping)
     in case matches of
            [] -> []
            ys -> [(p, number) | (p, _) <- matches]

getMatchesAll :: [String] -> String -> [(Int, Char)]
getMatchesAll words xs =
    let allMatches = map (getMatches xs) words
     in concat allMatches

getExtremeMatches :: [(Int, Char)] -> [(Int, Char)]
getExtremeMatches [] = []
getExtremeMatches xs =
    let leftMatch = head $ sortBy (compare `on` fst) xs
        rightMatch = last $ sortBy (compare `on` fst) xs
     in [leftMatch, rightMatch]

compareDigitMatches :: (((Int, Char), (Int, Char)), [(Int, Char)]) -> (Char, Char)
compareDigitMatches (((dpl, dcl), (dpr, dcr)), []) = (dcl, dcr)
compareDigitMatches (((dpl, dcl), (dpr, dcr)), [(mpl, mcl), (mpr, mcr)])
    | dpl < mpl && dpr > mpr = (dcl, dcr)
    | dpl < mpl && dpr < mpr = (dcl, mcr)
    | dpl > mpl && dpr > mpr = (mcl, dcr)
    | dpl > mpl && dpr < mpr = (mcl, mcr)

addCalValues :: (Char, Char) -> Int
addCalValues (a, b) = read ([a] ++ [b]) :: Int

main = do
    -- part 1
    contents <- IO.readFile "input"
    let lines = splitFile contents
        valueSums = map (addCalValues . getFromDigits) lines
        fstAnswer = sum valueSums

    print "Answer part 1:"
    print fstAnswer

    -- part 2
    let digitPos = map getFromDigitsPos lines
        numbers = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
        matchPos = map (getExtremeMatches . getMatchesAll numbers) lines
        zipped = zip digitPos matchPos
        compared = map compareDigitMatches zipped
        sndAnswer = sum $ map addCalValues compared
    print "Answer part 2:"
    print sndAnswer
