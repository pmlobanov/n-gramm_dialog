module Lib
    ( parseText) where

import Control.Applicative
import Data.Char
import Data.List 
import Data.Maybe
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
--createdictionary :: [(String,String)]-> Map.Map String String
--createdictionary (s:ss) =   
{-
-- Функция для добавления значения в список по ключу в словаре
addToMap :: (Ord k) => k -> v -> Map.Map k [v] -> Map.Map k [v]
addToMap key value = Map.insertWith (++) key [value]
addToMap _ "" = if Map.lookup 
-}


addPair :: Pair -> Map String [String] -> Map String [String]
addPair (k, v) m
  | all (== ' ') v = Map.insert k [] m
  | otherwise =  Map.insertWith (\new old -> nub (new ++ old)) k [v] m

buildMap :: [Pair] -> Map String [String]
buildMap = foldr addPair Map.empty


{-
addToMap :: (Ord k) => k-> String -> Map.Map k [String] -> Map.Map k [String]
addToMap key value m
  | value == "1" =  case Map.lookup key m of
                      Nothing -> Map.insert key [value] m
                      Just existingValue -> Map.insertWith (++) key (existingValue ++ ["2"]) m --if existingValue == [""] then Map.insert key [value] m else m
  | otherwise =  Map.insertWith (++) key [value] m

-- Функция для генерации словаря из списка кортежей
generateDictionary :: (Ord k) => [(k, String )] -> Map.Map k [String ]
generateDictionary = foldr (uncurry addToMap) Map.empty
-}
