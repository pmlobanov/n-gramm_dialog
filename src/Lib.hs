module Lib
    ( parseText) where

import Control.Applicative
import Data.Char
newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok],a) }

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
isleftSymbol = satisfy (\c -> (isPunctuation c  || isSymbol c ) && not (c `elem` ".!?:;()" ))


wordParser :: Parser Char Char
wordParser = optional isleftSymbol *> satisfy (\c -> not (isPunctuation c) && not (isDigit c)) <* optional isleftSymbol

sentenceDelimiter :: Parser Char Char
sentenceDelimiter = satisfy (`elem` ".!?:;()")

sentenceParser :: Parser Char String
sentenceParser = some wordParser <* optional (some (satisfy isDigit)) <* optional ( satisfy isSeparator) <* optional isleftSymbol 

parseText :: Parser Char [String]
parseText = some (sentenceParser <* sentenceDelimiter <* optional ( satisfy isSeparator)) 

getparsedText :: String -> [String]
getparsedText input = case runParser parseText input of
    Just (rest, sentences) -> sentences
    Nothing -> []

-- test text "a b, c d e! b c d? e b c# a ^d. a f; f."