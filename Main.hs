import System.IO
import Lexer

main = do
  repl

repl = do
  putStr "yali> "
  hFlush stdout 
  eof <- isEOF
  if eof then putStrLn ""
    else do
      line <- getLine
      if (line == "quit") then return ()
        else do
          putStrLn $ show (lexer line)
          repl
