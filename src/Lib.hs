module Lib
    ( parseText) where

import Control.Applicative
import Data.Char
import Data.List 
import System.Random (randomRIO)
import Data.Map (Map)
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
wordParser = optional isleftSymbol *> satisfy (\c -> not (isPunctuation c) && not (isDigit c) {-&& not (isSeparator c)-}) <* optional isleftSymbol

sentenceDelimiter :: Parser Char Char
sentenceDelimiter = satisfy (`elem` ".!?:;()")

sentenceParser :: Parser Char String
sentenceParser = some wordParser <* optional (some (satisfy isDigit)) <* optional ( satisfy isSeparator) <* optional isleftSymbol 

parseText :: Parser Char [String]
parseText = some (sentenceParser <* sentenceDelimiter <* optional ( satisfy isSeparator)) 

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

getUniqueWords :: String -> [String]
getUniqueWords text= nub (mconcat (textConvert (getparsedText text))) 

bigrams :: [String] -> [(String, String)]
bigrams [] = []
bigrams [x] = [(x," ")]
bigrams (x:xs) = (x, head xs) : bigrams  xs

trigrams :: [String] -> [(String, String)]
trigrams xs  = [(x, y ++ " " ++ z) | (x:y:z:_) <- tails xs]


qtrigrams :: [String] -> [(String, String)]
qtrigrams xs  = [(x ++ " " ++ y, z) | (x:y:z:_) <- tails xs]


trigramsFromLists :: [[String]] -> [(String, String)]
trigramsFromLists xs=  concatMap trigrams (filter (\ys -> length ys >= 3) xs) ++ concatMap qtrigrams (filter (\ys -> length ys >= 3) xs) ++ concatMap bigrams (filter (\ys -> length ys >= 2) xs)

addPair :: Pair -> Map String [String] -> Map String [String]
addPair (k, v) m
  | all (== ' ') v = Map.insert k [] m
  | otherwise =  Map.insertWith (\new old -> nub (new ++ old)) k [v] m

buildMap :: [Pair] -> Map String [String]
buildMap = foldr addPair Map.empty

-- переписать без do нотации    
randomElement :: [a] -> IO a
randomElement xs = randomRIO (0, length xs - 1) >>= \idx -> return (xs !! idx)

-- Функция для генерации случайной строки
generateRandomString :: Map String [String] -> String -> IO String
generateRandomString m key = generateRandomString' m key 15 [key]

generateRandomString' :: Map String [String] -> String -> Int -> [String] -> IO String
generateRandomString' _ _ 0 acc = return $ unwords (reverse acc)
generateRandomString' m key n acc = 
  case m Map.!? key of
    Nothing -> if n == 15 then return $ "Ключ " ++ key ++ " не найден в словаре."
                else return $  "Результат генерации: " ++ unwords (reverse acc) -- ++ show m-- (mconcat acc)
    
    Just [] -> return $ unwords (reverse acc) -- ++ key
    Just values -> randomElement values  >>= \next -> generateRandomString' m next (n - 1) (next : acc)


main :: IO ()
main = 
  let pairs = [("a","b c"),("b","c d"),("c","d e"),("b","c d"),("e","b c"),("b","c a"),("c","a d"),("a b","c"),("b c","d"),("c d","e"),("b c","d"),("e b","c"),("b c","a"),("c a","d"),("a","b"),("b","c"),("c","d"),("d","e"),("e"," "),("b","c"),("c","d"),("d"," "),("e","b"),("b","c"),("c","a"),("a","d"),("d"," "),("a","f"),("f"," ")]
      dictionary = buildMap pairs
  in putStrLn "Введите слово или пару слов:">>
     getLine >>= \input ->
     generateRandomString dictionary input >>= \result ->
     putStrLn result