{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module AParser3 where -- (Parser, runParser, satisfy, char, posInt))

import           Control.Applicative
import Control.Monad.State
import Control.Monad.Identity
import           Data.Char
import Data.Maybe
import SExpr

-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

type Parser a = StateT String Maybe a
runParser = runStateT

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
    (x:xs) <- get
    case p x of
        True -> put xs >> return x
        _ -> lift Nothing

posInt :: Parser Integer
posInt = do 
    xs <- get
    let (ns, rest) = span isDigit xs
    if null ns 
        then lift Nothing 
        else put rest >> return (read ns)

-- all the commented stuff is redundant because it's already done by StateT

-- inParser f = Parser . f . runParser

-- first :: (a -> b) -> (a,c) -> (b,c)
-- first f (x,y) = (f x, y)

-- instance Functor Parser where
--   fmap = inParser . fmap . fmap . first

-- instance Applicative Parser where
--   pure a = Parser (\s -> Just (a, s))
--   (Parser fp) <*> xp = Parser g where
--     g s = case fp s of
--       Nothing     -> Nothing
--       Just (f,s') -> runParser (f <$> xp) s'

-- instance Alternative (StateT String Maybe) where
--   empty = lift Nothing
--   -- Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2
--   -- p1 <|> p2 = do
--   --   xs <- get
--   --   let res = (<|>) (runParser p1 xs) (runParser p2 xs)
--   --   case res of
--   --       Nothing -> lift Nothing
--   --       _ -> lift $ fmap fst res
--   (<|>) = mplus

-- instance Monad Parser where
--     return = pure
--     Parser fp >>= f = Parser g where
--         g ss = case fp ss of
--             Nothing -> Nothing
--             Just (x, ss1) -> runParser (f x) ss1

-- the below is all in homework 11

char :: Char -> Parser Char
char c = satisfy (== c)

intOrUpperCase :: Parser ()
intOrUpperCase =  int <|> uc
    where
        int = pure (const ()) <*> posInt
        uc = pure (const ()) <*> satisfy isUpper

spaces :: Parser String
spaces = pure (const "") <*> (many $ satisfy isSpace)

-- applicative style is much better for this
ident :: Parser Atom
ident = do
    let alpha = I <$> (liftA2 (:) (satisfy isAlpha) (many $ satisfy isAlphaNum))
        num = N <$> posInt
    spaces
    i <- alpha <|> num
    spaces
    return i
-- ident = spaces *> (alpha <|> num) <* spaces
--     where
--         alpha = I <$> (liftA2 (:) (satisfy isAlpha) (many $ satisfy isAlphaNum))
--         num = N <$> posInt

parseSExpr :: Parser SExpr
parseSExpr = identS <|> combS
    where
        identS = A <$> ident
        combS = Comb <$> (skipChar '(' *> some parseSExpr <* skipChar ')')

skipChar :: Char -> Parser String
skipChar c = spaces *> (pure (const "") <*> char c) <* spaces

se = "( lots of ( 5 spaces in ) this ( one ) )"




parseInt :: Parser Int
parseInt = spaces *> (read . concat <$> (some $ show <$> posInt))

parseFile :: Parser [[Int]]
parseFile = many parseLine

parseLine :: Parser [Int]
-- parseLine = do
    -- i <- parseInt
    -- j <- replicateM i parseInt
    -- return [i]
parseLine = parseInt >>= (\i -> replicateM i parseInt >> pure [i])



main = print $ runParser parseSExpr se -- "4 78 19 3 44 3 1 7 5 2 3 2"