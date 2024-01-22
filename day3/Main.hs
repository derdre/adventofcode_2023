-- to get unique characters of file: `fold -w1 input | sort | uniq`

import Data.Maybe
import qualified System.IO as IO
import Text.Regex.TDFA

sliceString :: Int -> Int -> String -> String
sliceString _ _ "" = ""
sliceString from to xs
    | from >= to = []
    | otherwise = take (to - from) (drop from xs)

sliceList :: Int -> Int -> [String] -> [String]
sliceList _ _ [] = []
sliceList from to xs
    | from >= to = []
    | otherwise = take (to - from) (drop from xs)

getRectangle :: [String] -> (Int, Int, Int, Int) -> String
getRectangle [] _ = []
getRectangle xs (r1, c1, r2, c2) =
    let r_max = length xs
        c_max = length $ head xs
        rows = sliceList (max 0 r1) (min r_max r2) xs
     in concatMap (sliceString (max 0 c1) (min c_max c2)) rows

getNumbers :: String -> [((Int, Int), String)]
getNumbers "" = []
getNumbers xs =
    let pos = getAllMatches (xs =~ "[0-9]+") :: [(Int, Int)]
        num = getAllTextMatches (xs =~ "[0-9]+") :: [String]
     in zip pos num

getStars :: String -> [Int]
getStars [] = []
getStars s =
    let pos = getAllMatches (s =~ "[*]") :: [(Int, Int)]
     in map fst pos

enum :: [a] -> [(Int, a)]
enum = zip [0 ..]

procLines :: [String] -> [(String, String)]
procLines [] = []
procLines xs =
    let rows = [(i, getNumbers x) | (i, x) <- enum xs]
        positions = concat [[((i - 1, l - 1, i + 2, l + w + 1), num) | ((l, w), num) <- ns] | (i, ns) <- rows]
        blobs = [(getRectangle xs p, s) | (p, s) <- positions]
     in filter (hasSymbol . fst) blobs

getNumRect :: [String] -> [((Int, Int, Int, Int), String)]
getNumRect [] = []
getNumRect xs =
    let rows = [(i, getNumbers x) | (i, x) <- enum xs]
     in concat [[((i - 1, l - 1, i + 1, l + w), num) | ((l, w), num) <- ns] | (i, ns) <- rows]

hasSymbol :: String -> Bool
hasSymbol s = s =~ "[#%&*+/=@$-]" :: Bool

getStarRect :: [String] -> [(Int, Int, Int, Int)]
getStarRect [] = []
getStarRect xs =
    let rows = [(i, getStars x) | (i, x) <- enum xs]
     in concat [[(i - 1, s - 1, i + 1, s + 1) | s <- ss] | (i, ss) <- rows]

hasIntersection :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
hasIntersection (r11, c11, r12, c12) (r21, c21, r22, c22) =
    (c11 < c22) && (c12 > c21) && (r11 < r22) && (r12 > r21)

filterGear :: [(Int, Int, Int, Int)] -> [((Int, Int, Int, Int), String)] -> [[String]]
filterGear stars nums =
    let intersections = [[if hasIntersection s p then Just n else Nothing | (p, n) <- nums] | s <- stars]
     in [catMaybes is | is <- intersections]

multStar :: [String] -> Int
multStar s = product $ map (read :: String -> Int) s

main = do
    -- example 1
    example <- IO.readFile "input_ex1"
    let listLines = lines example
        symbolLines = procLines listLines
        numSum = sum $ map (read . snd) symbolLines

    print "Answer example 1:"
    print numSum

    -- example 2
    let starPos = getStarRect listLines
        numPos = getNumRect listLines
        filtered = filterGear starPos numPos
        pairs = filter ((== 2) . length) filtered
        multStars = map multStar pairs
        sumGears = sum multStars

    print "Answer example 2:"
    print sumGears

    -- part 1
    contents <- IO.readFile "input"
    let listLines = lines contents
        symbolLines = procLines listLines
        numSum = sum $ map (read . snd) symbolLines

    print "Answer 1:"
    print numSum

    -- part 2
    let starPos = getStarRect listLines
        numPos = getNumRect listLines
        filtered = filterGear starPos numPos
        pairs = filter ((== 2) . length) filtered
        multStars = map multStar pairs
        sumGears = sum multStars

    print "Answer 2:"
    print sumGears
