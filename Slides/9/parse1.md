# Kombinatory parsujące

Kod w katalogu Code/Parse1

Możemy zdefiniować typ parserów na przykład jako

~~~~ {.haskell}
newtype Parser a = Parser { runParser :: 
                       String -> [(a,String)] }
~~~~

albo, używając transformatorów monad

~~~~ {.haskell}
type Parser a = StateT [Char] (ErrorT String []) a
~~~~

oraz kombinatory (funkcje) reprezentujące elementarne parsery i~sposoby łączenia parserów:

~~~~ {.haskell}
item :: Parser Char
eof :: Parser ()
(<|>) :: Parser a -> Parser a -> Parser a
satisfy :: (Char->Bool) -> Parser Char
char :: Char -> Parser Char
char x = satisfy (==x)
many, many1 :: Parser a -> Parser [a]\
~~~~

# Parsec

Przeanalizujmy teraz jak zbudowana jest biblioteka Parsec, 
najpierw w wersji 2, potem 3

Wersja 2 oparta jest na monadzie stanu, w pierwszym przybliżeniu:

~~~~ {.haskell}
-- Code/Parse1/MyParsec2a
newtype Parser a = Parser { runParser :: State -> Reply a }
-- Poniższe typy będą rozszerzane w kolejnych wersjach
type State = [Char]
data Reply a = Ok a State | Error ParseError  
type ParseError = String
~~~~

~~~~ {.haskell}
p3 :: Parser String
p3 = p <|> q where
  p = char 'p' >> eof >> return "p"
  q = char 'p' >> char 'q' >> eof >> return "q"
test3 = runParser p3 "pq"

*MyParsec2a.Prim> test3
Ok "q" ""
~~~~


# Podstawowe kombinatory

~~~~ {.haskell}
item = Parser item0 where
  item0 :: State -> Reply Char
  item0 [] = Error $ unexpected "EOF"
  item0 (x:xs) = Ok x xs

eof = Parser eof' where
  eof' [] = Ok () []
  eof' _ = Error (expected "EOF")

char :: Char -> Parser Char
char c = (satisfy (==c)) 

satisfy p = Parser sat' where 
  sat' []    = Error (expected "EOF") -- or check (p EOF)
  sat' (a:s) = if (p a) then Ok a s else Error (unexpected $ show a) 
~~~~

# Sekwencjonowanie: monada

~~~~ {.haskell}
instance Monad Parser where
  return a = Parser $ \s -> Ok a s
  
  m >>= k = Parser (bind m k) where 
    bind (Parser f) k s = case f s of
      Ok a s' -> runParser (k a) s'
      Error e -> Error e

p0 = return ()
test0 = testP p0 ""

p2 = item >> item
test1 = testP p2 "" -- expect "EOF"
test2 = testP p2 "abc" -- "'b', c"

~~~~

# Zero i Plus

~~~~ {.haskell}
parserZero :: Parser a
parserZero = Parser $ \s -> Error unknownError

parserPlus :: Parser a -> Parser a -> Parser a
parserPlus p q = Parser $ \s -> case runParser p s of
  Error e -> runParser q s
  ok -> ok
  
(<|>) = parserPlus  

instance MonadPlus Parser where
  mzero = parserZero
  mplus = parserPlus
~~~~

# Ciągi

~~~~ {.haskell}
space :: Parser Char
space = satisfy isSpace

many, many1 :: Parser a -> Parser [a]
many p  = many1 p <|> return []
many1 p = do { x <- p ; xs <- many p; return (x:xs) }

skipMany :: Parser a -> Parser ()
skipMany p = many p >> return ()  -- można efektywniej
spaces p = skipMany space

~~~~

# Przykład

Za "Real World Haskell": http://book.realworldhaskell.org/read/using-parsec.html

~~~~ {.haskell}
csvFile :: Parser [[String]]
csvFile = 
    do result <- many line
       eof
       return result
~~~~

Ale można lepiej:

~~~~ {.haskell}
endBy  :: Parser a -> Parser b -> Parser a
endBy p q = do {x <- p; q; return x}

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end  = scan where
  scan = do{ end; return [] }
      <|>
         do{ x <- p; xs <- scan; return (x:xs) }

csvFile = manyTill eof line
line = cells `endBy` eol
~~~~

# Jeszcze ciągi

Linia pliku CSV to ciąg komórek rozdzielonych przecinkami

~~~~ {.haskell}
cells :: Parser [String]
cells = 
    do first <- cellContent
       next <- remainingCells
       return (first : next)

remainingCells :: Parser [String]
remainingCells =
    (char ',' >> cells)
    <|> (return [])    
~~~~

...ale chcielibyśmy prościej:

~~~~ {.haskell}
cells = cellContent `sepBy` char ','

sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }
~~~~

# Ciągi ze znaczącymi separatorami

Klasyczny przykład - wyrażenia arytmetyczne

~~~~ {.haskell}
-- >  expr    = term   `chainl1` addop
-- >  term    = factor `chainl1` mulop
-- >  factor  = parens expr <|> integer
-- >
-- >  mulop   =   do{ symbol "*"; return (*)   }
-- >          <|> do{ symbol "/"; return (div) }
-- >
-- >  addop   =   do{ symbol "+"; return (+) }
-- >          <|> do{ symbol "-"; return (-) }

chainl1 :: Parser a -> Parser (a->a->a) -> Parser a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x
~~~~

# Lepsza obsługa błędów

~~~~ {.haskell}
p4 :: Parser Int
p4 = fmap digitToInt (digit)
test4a = testP p4 "7"
test4b = testP p4 "x"

-- >>> test4a
-- Ok 7 ""
-- >>> test4b
-- Error "unexpected 'x'"
~~~~

Chcielibyśmy, by komunikat o błędzie podawał: 

* gdzie wystąpił błąd
* czego oczekiwano...

Dla zrealizowania pierwszego postulatu, stan musi przechowywać bieżącą pozycję, np.

~~~~ {.haskell}
newtype State = State {stPos :: Pos, stInput :: String}
~~~~

* Ćwiczenie: zmodyfikuj MyParsec2a tak, aby przechowywał i raportował pozycje błędów.

# Lepsze raportowanie błędów

~~~~ {.haskell}
digit :: Parser Char
digit = satisfy isDigit <?> "digit"

-- *MyParsec2b Text.Parsec> test4b
-- Error (Expected ["digit"])
~~~~


# Lepsze raportowanie błędów

~~~~ {.haskell}
type ParseError = Message -- lepiej [Message]

data Message = 
  UnknownError String
  | Unexpected String
  | Expected [String]
               deriving Show

p <?> expected = label p expected

label p expected = Parser $ \st -> case runParser p st of
  Ok a st' -> Ok a st'
  Error e -> Error $ addExpected expected e
  
addExpected x (Expected xs) = Expected (x:xs)
addExpected x (UnknownError _) = Expected [x]
addExpected x (Unexpected _) = Expected [x]
~~~~

Ćwiczenie: zmodyfikuj swoje rozwiązanie poprzeniego ćwiczenia by działało jak Parsec:

~~~~
Prelude Text.Parsec> parse digit "" ""
Left (line 1, column 1):
unexpected end of input
expecting digit
~~~~

# Usprawnianie

# Parsec3: kontynuacje

~~~~ {.haskell}
newtype Parsec a = Parsec { unParser :: forall b.
                                 State
                              -> (a -> State -> b) --  cok
                              -> (ParseError -> b) --  cerr
                              -> b
                            }
                    
item' [] cok cerr = cerr (unexpected "EOF")
item' (x:xs) cok cerr = cok x xs
item = Parsec item'

eof :: a -> Parsec a
eof a = Parsec eof' where
  eof' [] cok cerr = cok a []
  eof' _ cok cerr = cerr (expected "EOF")
~~~~

