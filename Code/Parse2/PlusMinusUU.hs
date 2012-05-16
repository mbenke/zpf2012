{-# LANGUAGE FlexibleContexts, RankNTypes #-}
import Data.Char(digitToInt)
import Criterion.Main

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances hiding(Parser)
type Parser a = P (Str Char String LineColPos) a

digit :: Parser Char
-- digit = pSym ('0', '9')
digit = pDigit
char :: Char -> Parser Char
char = pSym 

gen 0 = "1"
gen n = ('1':'+':'1':'-':gen (n-1))


pNum :: Parser Int
pNum = fmap digitToInt digit

chainl1 = flip pChainl
pExp = pNum `chainl1` addop
addop   =  (+) <$ char '+'
       <<|> (-) <$ char '-' 
       
-- pEnd :: Parser [Error]
doparse :: Parser a -> String -> String -> a
doparse p name input = let 
  extp = ( (,) <$> p <*> pEnd) 
  str = (createStr (LineColPos 0 0 0) input)
  (a, errors) =  parse extp str 
  in a {-case errors of
     [] -> a
     (e:_) -> error $ show e
  -}
  
test n =  doparse pExp "gen" (gen n)

main = defaultMain 
       [ bench "gen 10000" $ whnf test 10000
       , bench "gen 100000" $ whnf test 100000
       ] 