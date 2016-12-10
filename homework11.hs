-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Home11 where
import SExpr
import AParser2
import Data.Char
import Control.Applicative
import Control.Monad

helper :: Parser a -> Parser [a] -> Parser [a]
helper p1 p2 = liftA2 (:) p1 p2 <|> pure mempty

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = foldr helper (pure []) $ repeat p

-- can actually do this with a custom instance of traversable using helper, then use sequencea

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = concat <$> sequenceA [pure <$> p, zeroOrMore p]
-- oneOrMore p = liftA2 (:) p (zeroOrMore p)

-- foldParsers :: [Parser a] -> Parser [a]
-- foldParsers [] = pure []
-- foldParsers (p:ps) = liftA2 (:) p (foldParsers ps) <|> pure mempty

spaces :: Parser String
spaces = pure (const "") <*> (zeroOrMore $ satisfy isSpace)

ident :: Parser Atom
ident = spaces *> (alpha <|> num) <* spaces
    where 
        alpha = I <$> (liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum))
        num = N <$> posInt


-- I AM A GODDAMN GENIUS
parseSExpr :: Parser SExpr
parseSExpr = identS <|> combS
    where
        identS = A <$> ident
        combS = Comb <$> (skipChar '(' *> oneOrMore parseSExpr <* skipChar ')')

skipChar :: Char -> Parser String
skipChar c = spaces *> (pure (const "") <*> char c) <* spaces

se = "( lots of ( 5 spaces in ) this ( one ) )"
-- Parsed se = Just $ Comb [A (I "lots"), A (I "of"), Comb [A (N 5), A (I "spaces"), A (I "in")], A (I "this"), Comb [A (I "one")]]


-- Implementing the Monad
instance Monad Parser where
    return = pure
    Parser fp >>= f = Parser g where
        g ss = case fp ss of
            Nothing -> Nothing
            Just (x, ss1) -> runParser (f x) ss1
    
jjj :: Parser (Parser a) -> Parser a
jjj (Parser g) = Parser h where
    h ss = case g ss of
        Nothing -> Nothing
        Just (f1, ss1) -> runParser f1 ss1

parseInt :: Parser Int
parseInt = spaces *> (read . concat <$> (oneOrMore $ show <$> posInt))

parseFile :: Parser [[Int]]
parseFile = zeroOrMore parseLine

parseLine :: Parser [Int]
-- parseLine = do
    -- i <- parseInt
    -- j <- replicateM i parseInt
    -- return [i]
parseLine = parseInt >>= (\i -> replicateM i parseInt >> pure [i])

main = print $ runParser (parseFile) "4 78 19 3 44 3 1 7 5 2 3 2"
-- main = print $ runParser (zeroOrMore iu) "ABghk"
