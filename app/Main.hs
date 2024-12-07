module Main where

import Lib 

main :: IO ()
main =
    putStrLn "\nChoose an action:" >>
    putStrLn "1. Text file preprocessing" >>
    putStrLn "2. Interact with model" >>
    putStrLn "3. Models dialogue" >>
    putStrLn "4. Exit" >>
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
      "3" ->
        putStrLn "Enter dictionary file name for first model:" >> 
        getLine >>= \ firstFileName ->
        putStrLn "Enter dictionary file name for second model" >> getLine >>=
        \ secondFileName-> part3 firstFileName secondFileName >> main
      "4" -> putStrLn "Good Buy!"
      _ -> putStrLn "No such action"

part1:: String -> String  -> IO ()
part1 inputFileName outputFileName = do 
    input <- readFile inputFileName
    let res = getparsedText input
    putStrLn "\nRecognized sentences:\n"
    printRes res
    let dictionary = buildMap(trigramsFromLists (textConvert res))
    writeFile outputFileName (dictToString $ dictionary)
    putStrLn "\n Dictionary based on recognized sentences are written to the file\n"
 
part2 :: String -> IO ()
part2 dictFilename = 
  parseFileToDict dictFilename >>= \dictionary ->
  generateSentences dictionary >>
  putStrLn "\nComplited\n"

part3::String -> String  -> IO ()
part3 firstFileName secondFileName =
  parseFileToDict firstFileName >>= \firstdictionary ->
  parseFileToDict secondFileName >>= \seconddictionary ->
  startDialogue firstdictionary seconddictionary >>
  putStrLn "\nComplited\n"
printRes:: [String] -> IO ()
printRes = foldr ((>>) . putStrLn) (return ())

