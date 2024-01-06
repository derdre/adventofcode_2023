import qualified Data.List.Split as Split
import qualified System.IO as IO
import Text.Regex.TDFA

splitLine :: String -> [String]
splitLine [] = [""]
splitLine row =
    let [game, cubeSets] = Split.splitOn ": " row
        splitSets = Split.splitOn "; " cubeSets
     in game : splitSets

getGameInt :: String -> Int
getGameInt [] = 0
getGameInt s =
    let n = s =~ "[0-9]+" :: String
     in read n

getFirst :: [String] -> String
getFirst [] = "0"
getFirst [x] = x

getRgb :: String -> [Int]
getRgb [] = [0, 0, 0]
getRgb s =
    let (_, _, _, matchR) = s =~ "([0-9]+) red" :: (String, String, String, [String])
        (_, _, _, matchG) = s =~ "([0-9]+) green" :: (String, String, String, [String])
        (_, _, _, matchB) = s =~ "([0-9]+) blue" :: (String, String, String, [String])
     in map (read . getFirst) [matchR, matchG, matchB]

isPossible :: [Int] -> Bool
isPossible [r, g, b] = (r <= 12) && (g <= 13) && (b <= 14)

lineCheck :: String -> Int
lineCheck s =
    let (game : splitSets) = splitLine s
        n = getGameInt game
        linePossible = all (isPossible . getRgb) splitSets
     in if linePossible then n else 0

getMaxRgb :: [Int] -> [String] -> [Int]
getMaxRgb acc [] = acc
getMaxRgb acc xs = foldl (\acc x -> zipWith max acc (getRgb x)) acc xs

powerRgb :: [Int] -> Int
powerRgb [r, g, b] = r * g * b

main = do
    -- example 1
    contents <- IO.readFile "input_ex1"
    let listLines = lines contents
        possibleGames = map lineCheck listLines

    print "Answer example 1:"
    print listLines
    print (sum possibleGames)

    -- example 2
    let listPower = map (powerRgb . getMaxRgb [0, 0, 0] . splitLine) listLines

    print "Answer example 2:"
    print (sum listPower)

    -- task 1
    contents <- IO.readFile "input"
    let listLines = lines contents
        possibleGames = map lineCheck listLines

    print "Answer 1:"
    print (sum possibleGames)

    -- task 2
    let listPower = map (powerRgb . getMaxRgb [0, 0, 0] . splitLine) listLines

    print "Answer 2:"
    print (sum listPower)
