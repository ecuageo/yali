
module Lexer where

{- comments start at # and continue to end of line -} 

import Data.Char 

data Token = EOF | 
             Variable String |
             Constant String |
             IntegerToken Integer |
             Operator Char |
             Divider Char |
             Error String
             deriving (Show, Eq)
                    
lexer :: String -> [Token]
lexer xs = let s = map removeComment (lines xs)
           in lexer' $ unlines s

removeComment :: String -> String
removeComment [] = []
removeComment ('#':xs) = []
removeComment (x:xs) = x : (removeComment xs)

-- basically uncommented code is passed here
lexer' :: String -> [Token]
lexer' [] = [EOF]
lexer' l@(x:xs) = if isSpace x then lexer' xs
                  else let (t, rest) = nextToken l
                       in (t:lexer' rest)

nextToken :: String -> (Token, String)
nextToken l@(x:xs) | isLower     x = let (first, rest) = readIdentifier l
                                     in (Variable first, rest)
                   | isUpper     x = let (first, rest) = readIdentifier l
                                     in (Constant first, rest)
                   | isDigit     x = let (first, rest) = readInteger l
                                     in (IntegerToken first, rest)
                   | isOperator  x = (Operator x, xs)  -- add a reader to interpret the operator more fully
                   | isDivider   x = (Divider x, xs)
                   | otherwise     = (Error [x], xs)
                                  
readIdentifier [] = ([], [])
readIdentifier l@(x:xs) = let (first, rest) = readLettersOrDigits xs
                              (first', rest') = underscoreTails rest
                              identifier = (x:first) ++ first'
                          in (identifier, rest')

readInteger l = let (first, rest) = readDigits l
                in ((read first :: Integer), rest)

readDigits xs = span isDigit xs                           

readLettersOrDigits :: String -> (String, String)
readLettersOrDigits xs = span isLetterOrDigit xs

underscoreTail [] = ([], [])
underscoreTail [x] = ([], [x])
underscoreTail l@(x:y:xs) = if isUnderScore x && isLetterOrDigit y
                            then let (first, rest) = readLettersOrDigits xs
                                 in (x:y:first, rest)
                            else ("", l)

underscoreTails :: String -> (String, String)
underscoreTails xs = let (first, rest) = underscoreTail xs 
                     in if null first then (first, rest)
                        else let (first', rest') = underscoreTails rest
                             in (first ++ first', rest')

isDivider c = c `elem` ";{}(),"

isOperator c = c `elem` "-+*/%\""

isUnderScore c = c == '_'

isLetterOrDigit c = isLetter c || isDigit c

-- token :: String -> Integer
-- token l@(x:xs) 
--   | '-' == x = token xs * (-1)
--   | isDigit x = read l :: Integer
--   | True =          
