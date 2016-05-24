module Lib
    ( ssvParser
    ) where

import Data.Maybe
import Data.List 

ssvParser :: IO ()
ssvParser = do 
 putStrLn "Please input Filepath."
 file <- getLine 
 text <- readFile file
 print $ parse ssv text
 putStrLn "Do you want to validate? [y/n]"
 ans <- getLine 
 if (ans == "y") || (ans == "Y") then print $ fmap (allRowsEq.fst) $ parse ssv text else putStrLn "Ok, bye."

newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) = p

class Monad m => MonadZero m where 
 mzero :: m a 

class MonadZero m => MonadPlus m where 
 mplus :: m a -> m a -> m a 

class Applicative f => Alternative f where
 empty :: f a
 (<|>) :: f a -> f a -> f a

instance Functor Parser where 
 fmap f p  = Parser (\cs -> [(f a,cs') | (a,cs') <- parse p cs])

instance Applicative Parser where 
 pure a =  Parser (\cs -> [(a,cs)])
 p <*> q = Parser (\cs -> [(f a,cs') | (f,cs') <- parse p cs, (a,cs') <- parse q cs ])

instance Monad Parser where 
 return a = Parser (\cs -> [(a,cs)])
 p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

instance MonadZero Parser where 
 mzero = Parser (\cs -> [])

instance MonadPlus Parser where 
 mplus p q = Parser (\cs -> parse p cs ++ parse q cs)

(+++) :: Parser a -> Parser a -> Parser a 
p +++ q = Parser (\cs -> case parse (mplus p q) cs of 
                                []     -> []
                                (x:xs) -> [x])

instance Alternative Parser where 
 empty = mzero
 (<|>) p1 p2 = Parser $ \xs -> case parse p1 xs of 
                                []  -> parse p2 xs 
                                _   -> parse p1 xs

item :: Parser Char 
item = Parser (\xs -> case xs of 
                         ""      -> []
                         (c:cs) -> [(c,cs)])

many :: Parser a -> Parser [a]
many p = many1 p +++ return [] 

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)} 

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = (end >> return []) <|> do {x <- p; xs <- (manyTill p end); return (x:xs)}

sat :: (Char -> Bool) -> Parser Char 
sat pred = do {c <- item; if pred c then return c else mzero} 

char :: Char -> Parser Char 
char c = sat (c ==)

string :: String -> Parser String
string ""     = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

---ssvParser---

{-
Hiermit lassen sich simple und hard parsen. 
Ich verstehe nicht, warum ssv bei problematic 
nach der zweiten Row abbricht.. oO 
-}

data SSV = CSV {rows :: [Row]} | TSV {rows :: [Row]} 
 deriving Show

data Row = Row {fields :: [FieldValue]}
 deriving Show 

data FieldValue = FVQ {value :: String} | FVB {value :: String}
 deriving Show 

ssv :: Parser SSV 
ssv =  csv <|> tsv

rowLength :: SSV -> [Int]
rowLength ssv = fmap (length.fields) $ rows ssv 

allRowsEq :: SSV -> (Either String Int)
allRowsEq ssv = if all (== head ls) ls == True then Right (head ls) else Left "Rows are unequal in length."
 where ls = rowLength ssv


---csvParser---

{-
Der CSV Parser wird der csv-EBNF nicht zu 100% gerecht: 
1. Ein eol innerhalb eines quoted-strings ist zugelassen. 
2. char ' ' am Ende eines bare-Strings sind zugelassen. 
3. char '\t' innerhalb eines bare-strings ist NICHT zugelassen. 
   Dadurch erwartet csv nach einem auf '\t' terminierten field-value 
   ein Komma und scheitert bei einem tsv-file. Ließe ich '\t' zu, 
   würde csv ein tsv-file mit einem field pro Zeile erfolgreich 
   parsen... – und das will ich ja nicht.
Generell wüsste ich gerne, wie man am Besten Fehlermeldung implementiert. 
-}

csv :: Parser SSV
csv = do {file <- (many1 row); return $ CSV file}

row :: Parser Row 
row = do {r <- fieldList; (char '\r' >> char '\n') <|> char '\n'; return (Row r)}

fieldList :: Parser [FieldValue] 
fieldList = do {f <- field; fs <- ff; return (f:fs)} 
 where ff = many $ do {char ','; f1 <- field; return f1}

field = do {many whitespace; fv <- fieldValue; many whitespace; return fv}

fieldValue :: Parser FieldValue 
fieldValue = qString <|> bString

qString :: Parser FieldValue
qString = do {char '"'; qs <- manyTill item (char '"'); return (FVQ qs)}

bString :: Parser FieldValue
bString = Parser f 
 where 
  f "" = []
  f l  = [(FVB (take (bEnd l) l), drop (bEnd l) l)]
          where bEnd xs = minimum $ catMaybes [(elemIndex ',' xs),(elemIndex '\n' xs),(elemIndex '\r' xs)]

whitespace :: Parser Char
whitespace = (char ' ') <|> (char '\t')

---tsvParser---

tsv :: Parser SSV
tsv = do {file <- (many1 rowT); return $ TSV file} 

rowT :: Parser Row
rowT = do {r <- fieldListT; (char '\r' >> char '\n') <|> char '\n'; return $ Row r}

fieldListT :: Parser [FieldValue] 
fieldListT = do {f <- fieldT; fs <- ff; return (f:fs)} 
 where ff = many $ do {char ',' <|> (char '\t'); f1 <- fieldT; return f1}

fieldT = do {many whitespaceT; fv <- fieldValueT; many whitespaceT; return fv}

fieldValueT :: Parser FieldValue 
fieldValueT = (qString) <|> (bStringT)

bStringT :: Parser FieldValue
bStringT = Parser f 
 where 
  f "" = []
  f l  = [(FVB (take (bEnd l) l), drop (bEnd l) l)]
          where bEnd xs = minimum $ catMaybes [(elemIndex ',' xs),(elemIndex '\n' xs),(elemIndex '\t' xs),(elemIndex '\r' xs)]

whitespaceT :: Parser Char
whitespaceT = (char ' ') 




