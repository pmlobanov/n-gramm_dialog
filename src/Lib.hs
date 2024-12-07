module Lib (getparsedText, textConvert, trigramsFromLists, generateRandomString, buildMap, dictToString, generateSentences, parseFileToDict, startDialogue) where

import Control.Applicative
import Data.Char
import Data.List 
import System.Random (randomRIO)
import Data.Map (Map)
import Data.Map (Map, fromListWith, toList, keys, insert, fromList, (!?))
import Text.Printf (printf)
import qualified Data.Map as Map

newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok],a) }
type Pair = (String, String)

instance Functor (Parser tok) where
    fmap g = Parser . (fmap . fmap . fmap) g . runParser

instance Applicative (Parser tok) where 
    pure x =Parser $ \s-> Just(s,x)
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of 
            Nothing       -> Nothing
            Just (xs', g) -> case v xs' of 
                Nothing        -> Nothing
                Just (xs'', x) -> Just (xs'', g x)

instance Alternative (Parser tok) where
    empty = Parser $ const Nothing
    
    Parser u <|> Parser v = Parser f where 
        f xs = case u xs of
         Nothing -> v xs
         z       -> z

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
    f (c:cs) | pr c = Just (cs,c)
    f _ = Nothing

isleftSymbol :: Parser Char Char
isleftSymbol = satisfy (\c -> (isPunctuation c  || isSymbol c) && not (c `elem` ".!?:;() " ))

wordParser :: Parser Char Char
wordParser = optional isleftSymbol *> satisfy (\c -> not (isPunctuation c) && not (isDigit c) {-&& not (isSeparator c)-}) <* optional (some (satisfy isDigit))  <* optional isleftSymbol

sentenceDelimiter :: Parser Char Char
sentenceDelimiter = satisfy (`elem` ".!?:;()")

sentenceParser :: Parser Char String
sentenceParser = some wordParser <* optional (some (satisfy isDigit)) <* optional ( satisfy isSeparator) <* optional isleftSymbol 

parseText :: Parser Char [String]
parseText = some (sentenceParser <* sentenceDelimiter <* optional (satisfy ( == '\n')) <* optional ( satisfy isSeparator)) 

getparsedText :: String -> [String]
getparsedText input = case runParser parseText input of
    Just (_, sentences) -> sentences
    Nothing -> []

-- test text "a b, c d e! b c d? e b c# a ^d. a f; f."

-- [["a","b","c","d","e"],["b","c","d"],["e","b","c","a","d"],["a","f"],["f"]]

--todo Написать функцию getnext getnext2, которая берет следующее слово после пробела.

wordParserClean :: Parser Char String
wordParserClean=  some (satisfy isAlpha)

sentenceParserClean :: Parser Char [String]
sentenceParserClean = some (wordParserClean <* optional (satisfy isSeparator))

textConvert :: [String] -> [[String]]
textConvert [] = []
textConvert (s:ss) = case runParser sentenceParserClean s of
    Just (_, sentences) -> sentences : textConvert ss
    Nothing -> []

bigrams :: [String] -> [(String, String)]
bigrams [] = []
bigrams [x] = [(x," ")]
bigrams (x:xs) = (x, head xs) : bigrams  xs

trigrams :: [String] -> [(String, String)]
trigrams xs  = [(x, y ++ " " ++ z) | (x:y:z:_) <- tails xs]

qtrigrams :: [String] -> [(String, String)]
qtrigrams [] = []
qtrigrams [_] = []
qtrigrams [x, y] = [(x ++ " " ++ y," ")]
qtrigrams (x1:xs)  = (x1 ++ " " ++ head xs, head (tail xs)) : qtrigrams xs

trigramsFromLists :: [[String]] -> [(String, String)]
trigramsFromLists xs=  concatMap trigrams (filter (\ys -> length ys >= 3) xs) ++ concatMap qtrigrams xs ++ concatMap bigrams (filter (\ys -> length ys >= 2) xs)

addPair :: Pair -> Map String [String] -> Map String [String]
addPair (k, v) m
  | all (== ' ') v = Map.insert k [] m
  | otherwise =  Map.insertWith (\new old -> nub (new ++ old)) k [v] m

buildMap :: [Pair] -> Map String [String]
buildMap = foldr addPair Map.empty
  
randomElement :: [a] -> IO a
randomElement xs = randomRIO (0, length xs - 1) >>= \idx -> return (xs !! idx)

-- Функция для преобразования словаря в строку для записи в файл
dictToString :: Map String [String] -> String
dictToString dict =
    let dictList = toList dict
        formatEntry (key, values) = printf "\"%s\" : %s" key (show values)
    in unlines (map formatEntry dictList)

-- Функция для генерации случайной строки
generateRandomString :: Map String [String] -> String -> IO String
generateRandomString m key = generateRandomString' m key 15 [key]

generateRandomString' :: Map String [String] -> String -> Int -> [String] -> IO String
generateRandomString' _ _ 0 acc = return $ unwords (reverse acc)
generateRandomString' m key n acc = 
  case m Map.!? key of
    Nothing -> if n == 15 then return $ "Key " ++ key ++ "is not contained in the dictionary"
                else return $  unwords (reverse acc) -- ++ show m-- (mconcat acc)
    
    Just [] -> return $ unwords (reverse acc) -- ++ key
    Just values -> randomElement values  >>= \next -> generateRandomString' m next (n - 1) (next : acc)

generateSentences :: Map String [String] -> IO ()
generateSentences dictionary = 
     putStrLn "Enter word or couple of words:">>
     getLine >>=
        \input ->
            generateRandomString dictionary input >>= \result ->
                putStrLn $ "Generation result: " ++ result

char :: Char -> Parser Char Char
char c = satisfy (== c)

spaces :: Parser Char String
spaces = many (satisfy isSpace)

quotedString :: Parser Char String
quotedString = char '"' *> many (satisfy (/= '"')) <* char '"'

listParser :: Parser Char [String]
listParser = char '[' *> spaces *> sepBy quotedString (spaces *> char ',' <* spaces) <* spaces <* char ']'

sepBy :: Parser tok a -> Parser tok sep -> Parser tok [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

pairParser :: Parser Char (String, [String])
pairParser = 
    (,) <$> (quotedString <* spaces <* char ':' <* spaces) <*> listParser

parseLine :: String -> Maybe (String, [String])
parseLine input = case runParser pairParser input of
    Just ("", pair) -> Just pair
    _ -> Nothing

parseFileToDict :: FilePath -> IO (Map String [String])
parseFileToDict path = 
    readFile path >>= \contents ->
    case  mapM parseLine (lines contents) of
        Just ps -> return (Map.fromList ps)
        Nothing -> return Map.empty

findword::Map String [String] -> [String] -> Maybe String  
findword dict words = 
  case dict Map.!? last words of
    Nothing -> findword dict (init words) 
    Just [] -> findword dict (init words) 
    Just text -> 
        (if length text >= 2 then
     return (last words)
        else
     findword dict (init words))

   

runDialog:: Map String [String] -> Map String [String] -> String -> Int -> IO()
runDialog _ _ _ 0 = putStrLn "Conersation ended"
runDialog ourMap oppMap firstWords n     = 
    generateRandomString ourMap firstWords >>= \ourResponse ->
    putStrLn ("Model 1: " ++ ourResponse) >>
    case findword oppMap (words ourResponse) of
        Nothing -> putStrLn "Model 2 has nothing to say."
        Just oppWord->
            generateRandomString oppMap oppWord >>= \oppResponse ->
                putStrLn ("Model 2: " ++ oppResponse) >>
                case  findword ourMap (words oppResponse)  of
                    Nothing -> putStrLn "Model 1 has nothing to say."
                    Just ourword -> runDialog ourMap oppMap ourword (n-1)

startDialogue :: Map String [String] -> Map String [String] -> IO ()
startDialogue dict1 dict2 = do
    putStrLn "Enter the initial word (or a couple of words):"
    startWords <- getLine
    putStrLn "Enter the depth of M messages:"
    depth <- readLn
    runDialog dict1 dict2 startWords depth