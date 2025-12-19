{-# LANGUAGE LambdaCase #-}
import Data.Bifunctor (Bifunctor(first))
import Control.Applicative
import Data.Char (isDigit, isSpace)

data ParserError =
  EndOfFile |
  UnexpectedCharacter Char
  deriving Show

newtype Parser a = Parser { runParser :: String -> Either ParserError (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input -> do
    (a, rest) <- p input
    return (f a, rest)

  -- fmap f (Parser p) = Parser $ fmap (first f) <$> p

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \input -> Right (a, input)

  -- pure a = Parser $ Right . (a,)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser pf) <*> p = Parser $ \input -> do
    (f, rest) <- pf input
    runParser (f <$> p) rest

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser p) >>= f = Parser $ \input -> do
    (a, rest) <- p input
    runParser (f a) rest

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const (Left EndOfFile)

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    either (\_ -> p2 input) Right $ p1 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> Left EndOfFile
  (c:cs) -> if p c then Right (c, cs) else Left $ UnexpectedCharacter c

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

digit :: Parser Char
digit = satisfy isDigit

integer :: Parser String
integer = some digit

whitespace :: Parser String
whitespace = many (satisfy isSpace)

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy separator element = liftA2 (:) element (many $ separator *> element) <|> pure []

data Json =
  JsonNull |
  JsonBool Bool |
  JsonNumber Double |
  JsonString String |
  JsonArray [Json] |
  JsonObject [(String, Json)] deriving Show

jsonNull :: Parser Json
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser Json
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ string "true"
    jsonFalse = JsonBool False <$ string "false"

jsonNumber :: Parser Json
jsonNumber = JsonNumber . read <$> integer

jsonString :: Parser Json
jsonString = JsonString <$> (quote *> many (satisfy (/= '"')) <* quote)
  where
    quote = char '"'

jsonArray :: Parser Json
jsonArray = JsonArray <$> (char '[' *> separateBy (char ',' *> whitespace) jsonValue <* char ']')

jsonValue :: Parser Json
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray

json :: String -> Either ParserError Json
json input = do
  (result, _) <- runParser jsonValue input
  return result

parse :: String -> IO (Either ParserError Json)
parse filename = do
  content <- readFile filename
  return $ json content