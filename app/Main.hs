module Main where

import Lib 

main :: IO ()
main =
    putStrLn "\nChoose an option:" >>
    putStrLn "1. Text file preprocessing" >>
    putStrLn "2. Interact with model" >>
    putStrLn "4. Models dialogue" >>
    putStrLn "5. Exit" >>
    getLine >>= \choice ->
    case choice of
      "1" ->
        putStrLn "Enter input file name:" >> 
        getLine >>= \ inputFileName ->
        putStrLn "Enter output file name:" >> getLine >>=
        \ outputFileName-> part1 inputFileName outputFileName >> main
      "2" ->  putStrLn "Enter dictionary file name:" >>
        getLine >>= part2 >>
        main
      "5" -> putStrLn "Good Buy!"

part1:: String -> String  -> IO ()
part1 inputFileName outputFileName = do 
    --readFile inputFileName >>= \input -> getparsedText input >> putStrLn "\nCompleted successfully!\n"
    input <- readFile inputFileName
   -- putStrLn input
    let res = getparsedText input
    --putStrLn (show res)
    putStrLn "\nRecognized sentences:\n"
    printRes res
    let dictionary = buildMap(trigramsFromLists (textConvert res))
    writeFile outputFileName (dictToString $ dictionary)
    putStrLn "\n Dictionary based on recognized sentences are written to the file\n"
    {-input2 <- readFile outputFileName
    putStrLn  input2
    putStrLn "\nRecognized sentences:\n"
    
    putStrLn "\nВведите слово или пару слов:">>
     getLine >>= \input -> 
     generateRandomString dictionary input >>= \result ->
     putStrLn result
    -}
part2 :: String -> IO ()
part2 dictFilename = 
  parseFileToDict dictFilename >>= \dictionary ->
  generateSentences dictionary 
  --generateSentences dictionary  
 -- putStrLn "\n Dictionary based on recognized sentences are written to the file\n"


printRes:: [String] -> IO ()
printRes = foldr ((>>) . putStrLn) (return ())

